-module(storage).
-compile(export_all).


create() -> 
	[].

%add a key value pair, return the updated store
add(Key, Value, Store) ->
	[{Key, Value} | Store].

% return a tuple {Key, Value} if found or else return false
lookup(Key, Store) ->
	lists:keyfind(Key, 1, Store). 


% return a tuple {Updated, Rest} where the updated store only contains the key value pairs requested and the rest are found in a list of key-value pairs
split(From, To, Store) -> 
	lists:partition(fun({Key, _}) -> key:between(Key, From, To) end, Store).

% add a list of key-value pairs to a store
merge(Entries, Store) -> 
	%io:format("Enteries: ~w, Store: ~w ~n",[length(Entries), length(Store)]),
    Tot=lists:merge(Entries,Store),
    %io:format("Total: ~w~n",[length(Tot)]),
    Tot.