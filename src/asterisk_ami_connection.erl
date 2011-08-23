%%%-------------------------------------------------------------------
%%% File : asterisk_ami_connection.erl
%%% Author : Alexandru Pirvulescu <sigxcpu@gmail.com>
%%% Description :
%%%
%%% Created : 21 Aug 2011
%%%-------------------------------------------------------------------

-module (asterisk_ami_connection).
-behaviour(gen_server).

-include("asterisk_ami.hrl").

%% API

-export(
	[
		start_link/3, start_link/4,
		add_listener/2, add_listener/3,
		login/1,
		ping/1,
		stop/1
	]).

% tests
-export([
	connection_test/0
]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_AMI_PORT, 5038).
-define(DEFAULT_SOCKET_TIMEOUT, 3000).
-define(DEFAULT_TIMEOUT, 3000).
-define(TCP_OPTIONS, [binary, {packet, line}, {send_timeout, ?DEFAULT_SOCKET_TIMEOUT}]).
-define(KVP_REGEX, "([a-zA-Z0-9]+):?\s*(.*)\r\n").
-define(REGEX_OPTS,  {capture, [1,2], binary}).

-record(state, {
	state,
	host,
	port,
	user,
	password,
	ami_version,
	event_listeners,
	socket,
	event_dict,
	action_id,
	action_listeners,
	connect_wait_pid
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, User, Password) ->
	start_link(Host, ?DEFAULT_AMI_PORT, User, Password).

start_link(Host, Port, User, Password) ->
	gen_server:start_link(?MODULE, [Host, Port, User, Password], []).

stop(Connection) ->
	gen_server:cast(Connection, stop).

add_listener(Connection, Pid) ->
	add_listener(Connection, Pid, []).

add_listener(Connection, Pid, EventsList) ->
	gen_server:call(Connection, {add_listener, Pid, EventsList}).

login(Connection) ->
	gen_server:call(Connection, login).

ping(Connection) ->
	gen_server:call(Connection, ping).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%% {ok, State, Timeout} |
%% ignore |
%% {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Port, User, Password]) ->
	{ok, #state{
		state = initial,
		host = Host,
		port = Port,
		user = User,
		password = Password,
		event_listeners = ae_ets_map:new(list_to_atom(lists:flatten([atom_to_list(event_listeners), "_", Host]))),
		action_listeners = ae_ets_map:new(list_to_atom(lists:flatten([atom_to_list(action_listeners), "_", Host]))),
		event_dict = ae_dict_map:new({host, Host}),
		action_id = 0
	}, ?DEFAULT_TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%% {reply, Reply, State, Timeout} |
%% {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, Reply, State} |
%% {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({add_listener, Pid, EventsList}, _From, State) ->
	% monitor the pid to catch its death
	erlang:monitor(process, Pid),
	ae_map_ets:put(State#state.event_listeners, Pid, EventsList),
	{reply, ok, State};

handle_call(connect, {_Ref, ConnectWaitPid}, State) when State#state.state == initial ->
	case gen_tcp:connect(State#state.host, State#state.port, ?TCP_OPTIONS, ?DEFAULT_SOCKET_TIMEOUT) of
		{ok, Socket} ->
			{noreply, State#state{socket = Socket, state = connected, connect_wait_pid = ConnectWaitPid}, ?DEFAULT_TIMEOUT};
		{error, Reason} ->
			{reply, {error, Reason}, State, ?DEFAULT_TIMEOUT}
	end;

handle_call(login, From, State) when State#state.state == initial ->
	% first, connect
	case gen_tcp:connect(State#state.host, State#state.port, ?TCP_OPTIONS, ?DEFAULT_TIMEOUT) of
		{ok, Socket} ->
			% save the listener to this action id
			% ae_ets_map:put(State#state.action_listeners, State#state.action_id, Pid),
			% perform login action
			LoginActionData = [
				{"Username", State#state.user},
				{"Secret", State#state.password}
			],
			%error_logger:info_msg("Sending action ~p", [LoginAction]),
			NewState = send_raw_action("Login", LoginActionData, From, State#state{socket = Socket}),
			% add socket to state
			NewStateWithSocket = NewState#state{socket = Socket},
			{noreply, NewStateWithSocket};
		{error, Reason} ->
			{reply, {error, Reason}, State, ?DEFAULT_TIMEOUT}
	end;

handle_call(ping, From, State) when State#state.state == receiving ->
	NewState = send_raw_action("Ping", [], From, State),
	{noreply, NewState, ?DEFAULT_TIMEOUT};


handle_call(Request, From, State) ->
 	error_logger:error_msg("Unhandled call ~p from ~p in state ~p~n", [Request, From, State#state.state]),
 	{reply, {error, invalidcommand}, State, ?DEFAULT_TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Msg, State) ->
	error_logger:error_msg("Unhandled cast ~p in State ~p", [Msg, State#state.state]),
	{noreply, State, ?DEFAULT_TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(timeout, State) when State#state.state == receiving ->
	% need to ping
	NewState = send_raw_action_async("Ping", [], State),
	{noreply, NewState, ?DEFAULT_TIMEOUT};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
	% one of our monitored processes exited
	% if it was an event listener, remove it from list to avoid sending useless events
	ae_ets_map:remove(State#state.event_listeners, Pid),
	{noreply, State, ?DEFAULT_TIMEOUT};

%% Socket events
handle_info({tcp, Socket, <<"\r\n">>}, State) when Socket == State#state.socket ->
	% we've received a full packet
	% error_logger:info_msg("Received full packet: ~p", [dict:to_list(State#state.event_dict)]),
	case ae_dict_map:get(State#state.event_dict, actionid) of
		undefined ->
			% standard event
			NewState = handle_end_of_packet(State);
		_ActionId ->
			NewState = handle_action_response(State)
	end,
	{noreply, NewState, ?DEFAULT_TIMEOUT};

handle_info({tcp, Socket, Line}, State) when Socket == State#state.socket ->
	NewState = handle_event_line(Line, State),
	{noreply, NewState, ?DEFAULT_TIMEOUT};

handle_info({tcp_closed,Socket}, State) when Socket == State#state.socket ->
	{stop, disconnected, State};

%% End of socket events

handle_info(Info, State) ->
	error_logger:error_msg("Received unhandled info ~p in state ~p", [Info, State#state.state]),
	{noreply, State, ?DEFAULT_TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


handle_event_line(<<"Asterisk Call Manager/", AmiVersion/binary>>, State) when State#state.state == initial ->
	% this must be the AMI version string
	{match, [Version]} = re:run(AmiVersion, "(.*)\r\n", [{capture, [1], binary}]),
	State#state{state = receiving, ami_version = Version};

handle_event_line(LineWithCRLF, State) when State#state.state == receiving ->
	% an event line
	{match, [Key, Value]} = re:run(LineWithCRLF, "(.*)\\s*:\\s*(.*)\r\n", [{capture, [1,2], binary}]),
	KeyAtom = list_to_atom(string:to_lower(binary_to_list(Key))),
	NewEventDict = ae_dict_map:put(State#state.event_dict, KeyAtom, Value),
	State#state{event_dict = NewEventDict}.

handle_end_of_packet(State) when State#state.state == receiving ->
	% add host to our event
	EventDict = State#state.event_dict,
	io:format("Sending event ~p~n", [dict:to_list(EventDict)]),
	EventType = ae_dict_map:get(EventDict, event),
	% we need to notify our listeners now based on their subscriptions
	lists:foreach(
		fun(X) ->
			case receives_event(EventType, ae_ets_map:get(State#state.event_listeners, X)) of
				true ->
					X ! {asterisk_event, ae_dict_map:get(EventDict, event), EventDict};
				false ->
					ok
			end
		end, ae_ets_map:keys(State#state.event_listeners)),
	State#state{event_dict = ae_dict_map:new({host, State#state.host})}.

handle_action_response(State) when State#state.state == receiving ->
	EventDict = State#state.event_dict,
	ActionId = list_to_integer(binary_to_list(ae_dict_map:get(EventDict, actionid))),
	case ae_ets_map:get(State#state.action_listeners, ActionId) of
		undefined ->
			% error_logger:error_msg("Undefined action listener for action id ~p", [ActionId]);
			% this was an async action
			ok;
		{Action, PidRef} ->
			Response = parse_response(Action, EventDict),
			%error_logger:info_msg("Sending reply to pid ~p: ~p", [PidRef, dict:to_list(EventDict)]),
			gen_server:reply(PidRef, Response)

	end,
	State#state{event_dict = ae_dict_map:new({host, State#state.host})}.


receives_event(_EventType, []) -> true;
receives_event(EventType, EventsList) -> lists:member(EventType, EventsList).

send_raw_action_async(Action, ActionData, State) when State#state.state == initial orelse State#state.state == receiving ->
	send_raw_action(Action, ActionData, undefined, State).

send_raw_action(Action, ActionData, From, State) when State#state.state == initial orelse State#state.state == receiving ->
	ActionId = State#state.action_id,
	WithAddedActionIdAndAction = lists:flatten([
		{"Action", Action},
		{"ActionID", ActionId},
		ActionData
	]),

	% build a big binary to send
	ActionPacket = lists:foldl(
		fun({K, V}, Acc) ->
			BinK = ae_util:make_binary(K),
			BinV = ae_util:make_binary(V),
			<<Acc/binary, BinK/binary, ": ", BinV/binary, "\r\n">>
		end, <<>>, WithAddedActionIdAndAction),
	Socket = State#state.socket,
	ok = gen_tcp:send(Socket, <<ActionPacket/binary, "\r\n">>),
	% save the caller
	case From of
		undefined ->
			ok;
		From ->
			ae_ets_map:put(State#state.action_listeners, ActionId, {Action, From})
	end,
	State#state{action_id = ActionId + 1}.

parse_response("Login", EventDict) ->
	case ae_dict_map:get(EventDict, response) of
		<<"Success">> ->
			{ok, ae_dict_map:get(EventDict, message)};
		_Other ->
			{error, ae_dict_map:get(EventDict, message)}
	end;

parse_response("Ping", EventDict) ->
	{ok, ae_dict_map:get(EventDict, timestamp)}.

%% Tests


test_init() ->
	start_link("asterisk-dev", "asterisk", "astnetmon1").

connection_test() ->
	{ok, Connection} = test_init(),
	LoginResponse = login(Connection),
	error_logger:info_msg("~p~n", [LoginResponse]),
	timer:sleep(10000),
	PingResponse = ping(Connection),
	error_logger:info_msg("~p~n", [PingResponse]),
	stop(Connection).

