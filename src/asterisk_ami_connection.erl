%%%-------------------------------------------------------------------
%%% File : asterisk_ami_connection.erl
%%% Author : Alexandru Pirvulescu <sigxcpu@gmail.com>
%%% Description :
%%%
%%% Created : 21 Aug 2011
%%%-------------------------------------------------------------------

-module (asterisk_ami_connection).
-behaviour(gen_server).

%% API

-export([
	start_link/3, start_link/4,
	add_listener/2, connect/1, login/1
	]).

% tests
-export([
	connection_test/0
]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_AMI_PORT, 5038).
-define(TCP_OPTIONS, [binary, {packet, line}]).
-define(DEFAULT_TIMEOUT, 3000).

-record(state, {
	state,
	host,
	port,
	user,
	password,
	event_listeners,
	socket
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

add_listener(Connection, Pid) ->
	gen_server:call(Connection, {add_listener, Pid}).

connect(Connection) ->
	gen_server:call(Connection, connect).

login(Connection) ->
	gen_server:cast(Connection, login).

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
		event_listeners = []
	}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%% {reply, Reply, State, Timeout} |
%% {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, Reply, State} |
%% {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({add_listener, Pid}, _From, State) ->
	% monitor the pid to catch its death
	erlang:monitor(process, Pid),
	{reply, ok, State#state{event_listeners = [Pid] ++ State#state.event_listeners}};

handle_call(connect, _From, State) when State#state.state == initial ->
	case gen_tcp:connect(State#state.host, State#state.port, ?TCP_OPTIONS, ?DEFAULT_TIMEOUT) of
		{ok, Socket} ->
			Reply = ok,
			NewState = State#state{socket = Socket, state = connected};
		{error, Reason} ->
			Reply = {error, Reason},
			NewState = State
	end,
	{reply, Reply, NewState};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(Msg, State) ->
	error_logger:error_msg("Unhandled cast ~p in State ~p", [Msg, State#state.state]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
	% one of our monitored processes exited

	% if it was an event listener, remove it from list to avoid sending useless events
	EventListeners = [ X || X <- State#state.event_listeners, not X == Pid],
	{noreply, State#state{event_listeners = EventListeners}};

handle_info(Info, State) ->
	error_logger:info_msg("Received unhandled info ~p in state ~p", [Info, State#state.state]),
	{noreply, State}.

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

connection_test() ->
	{ok, Connection} = start_link("asterisk-dev", "asterisk", "astnetmon1"),
	ok = connect(Connection).
