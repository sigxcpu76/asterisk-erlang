-module (ae_dict_map).

-export([
	new/0, new/1,
	put/3,
	get/2
]).

new() -> dict:new().

new({K, V}) ->
	put(new(), K, V);

new([]) ->
	new();

new(List) when is_list(List) ->
	lists:foldl(
		fun({K, V}, Acc) ->
			put(Acc, K, V)
		end
		, new(), List).

put(D, K, V) ->
	dict:store(K, V, D).

get(D, K) ->
	case dict:find(K, D) of
		error ->
			undefined;
		{ok, [V]} ->
			V;
		{ok, V} ->
			V
	end.



