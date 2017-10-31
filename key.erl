-module(key).
-export([generate/0,between/3]).

generate() ->
    rand:uniform(1000000000).
 
between(Key, From, To) ->
    if
    From < To -> 
    	(From < Key) and (Key =< To);
    
    To < From -> 
    	(From < Key) or (Key =< To);

    From == To ->  
    	true
    end.