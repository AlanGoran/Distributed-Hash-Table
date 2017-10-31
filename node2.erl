-module(node2).
-compile(export_all).
 
-define(Stabilize, 100).
-define(Timeout, 10000).

start(Id) ->
    start(Id, nil).
 
start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).
 
init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create()).% implements message handling


% If we are the first node 
connect(Id, nil) ->
    {ok, {Id, self()}}; % If we’re all alone we are of course our own successors.

% If we are trying to connect to an existing ring
connect(Id, Peer) ->
    Qref = make_ref(), % create unique reference
    Peer ! {key, Qref, self()},
    receive
    {Qref, Skey} ->
        {ok, {Skey, Peer}}
    after ?Timeout ->
        io:format("Time out: no response~n",[])
    end. 



node(Id, Predecessor, Successor, Store) ->
    receive
    {key, Qref, Peer} ->
        Peer ! {Qref, Id},
        node(Id, Predecessor, Successor, Store);
 
    {notify, New} ->
        {Pred, Store1} = notify(New, Id, Predecessor, Store),
        node(Id, Pred, Successor, Store1);
 
    {request, Peer} ->
        request(Peer, Predecessor),
        node(Id, Predecessor, Successor, Store);
 
    {status, Pred} ->
        Succ = stabilize(Pred, Id, Successor),
        node(Id, Predecessor, Succ, Store);
    
    % when process receives stabalize message it will call stabilize/1
    stabilize ->
        stabilize(Successor),
        node(Id, Predecessor, Successor, Store);

    probe ->
	    io:format("Probe initialized ~n"),
	    create_probe(Id, Successor),
        node(Id, Predecessor, Successor, Store);
 
	{probe, Id, Nodes, T} ->
        io:format("Probe should stop ~n"),
        remove_probe(T, Nodes),
        node(Id, Predecessor, Successor, Store);
	 
    {probe, Ref, Nodes, T} ->
        forward_probe(Ref, T, Nodes, Id, Successor),
        node(Id, Predecessor, Successor, Store);
        
    {add, Key, Value, Qref, Client} ->
        Store1 = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Store1);
    {lookup, Key, Qref, Client} ->
        lookup(Key, Qref, Client, Id,Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Store);
 
    {handover, Elements} ->
        Merged = storage:merge(Elements, Store),
        node(Id, Predecessor, Successor, Merged);

    {error,Error} ->
        io:format("strange message ~w~n", [Error]),
        node(Id, Predecessor, Successor, Store); 
	stop ->
    	ok
	end.

% send a request message to its successor.
stabilize({_, Spid}) ->
    Spid ! {request, self()}.

% The Pred argument is ours successors current predecessor. 
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->  % If nil we should inform it about our existence.
            Spid ! {notify,{Id, self()}},
            {Skey, Spid};
        {Id, _} -> % If it is pointing back to us we don’t have to do anything.
            {Skey, Spid};
        {Skey, _} -> % If it is pointing to itself we should of course notify it about our existence.
            Spid ! {notify,{Id, self()}},
            {Skey, Spid};
            
        %If it’s pointing to another node we need to be careful. 
        %The question is if we are to slide in between the two nodes 
        %or if we should place ourselves behind the predecessor. 
        %If the key of the predecessor of our successor (Xkey) is between
        %us and our successor we should of course adopt this node as our 
        %successor and run stabilization again. 
        %If we should be in between the nodes we inform our successor of 
        %our existence.
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true -> 
                    Xpid ! {request, self()},
                    Pred;
                false -> 
                    Spid ! {notify, {Id,self()}},
                    {Skey, Spid}
            end 
    end.

% called when a node is created
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

% Informing the peer about predecessor
request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
    nil ->
        Keep = handover(Id, Store, Nkey, Npid),
        {{Nkey,Npid}, Keep};
    {Pkey, _} ->
        case key:between(Nkey, Pkey, Id) of
        true ->
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey,Npid}, Keep};
        false ->
            {Predecessor, Store}
        end
    end.

create_probe(Id, {_, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.
 
forward_probe(Ref,Time,Nodes,Id,{_, Spid}) ->
    io:format("~w: Forwarded ~n", [Id]),
    Spid ! {probe, Ref, Nodes ++ [Id], Time }.
 
remove_probe(Time, Nodes) ->
    Tot_Time = erlang:system_time(micro_seconds) - Time,
    Nodes_visited = length(Nodes),
    io:format("Iteration Time: ~w, visited ~w nodes ~n",[Tot_Time,Nodes_visited]).



handover(Id, Store, Nkey, Npid) ->
    {Rest, Keep} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.
 
% 1. determine if our node is the node that should take care of the key.
% 2. A node will take care of all keys from (but not including) the 
%    identifier of its predecessor to (and including) the identifier of itself.
% 3. If we are not responsible we simply send a add message to our successor.
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
    true ->
        Client ! {Qref, ok},
        storage:add(Key, Value, Store);
    false ->
        Spid ! {add, Key, Value, Qref, Client},
        Store
 
    end.

% 1. determine if we are responsible for the key
% 2. If so we do a simple lookup in the local store and then send the reply to the requester. 
% 3. If it is not our responsibility we simply forward the request. 
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
    true ->
        Result = storage:lookup(Key, Store),
        Client ! {Qref, Result};
    false ->
        {_, Spid} = Successor,
        Spid ! {lookup, Key, Qref, Client} 
    end.
