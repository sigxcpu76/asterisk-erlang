-module (ae_util).

-export([
	make_binary/1,
	proplist_to_varstring/1
]).

make_binary(X) when is_binary(X) -> X;
make_binary(X) when is_list(X) -> list_to_binary(X);
make_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
make_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
make_binary(X) when is_float(X) -> list_to_binary(float_to_list(X)).

proplist_to_varstring([]) ->
	[];

proplist_to_varstring([{K, V}]) ->
	[K, "=", V];

proplist_to_varstring([{K, V}|T]) ->
	[K, "=", V, $&] ++ proplist_to_varstring(T).

