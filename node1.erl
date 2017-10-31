-module(node1).
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
    node(Id, Predecessor, Successor). % implements message handling


% If we are the first node 
connect(Id, nil) ->
    {ok, {Id, self()}}; % If we’re all alone we are of course our own successors.

% If we are trying to connect to an existing ring
connect(_, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
	        {ok, {Skey, Peer}}
    after ?Timeout ->
            io:format("Time out: no response~n",[])
	end.


node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);

        % when process receives stabalize message it will call stabilize/1
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);

        probe ->
	        io:format("Probe initialized ~n"),
	        create_probe(Id, Successor),
	        node(Id, Predecessor, Successor);
 
	    {probe, Id, Nodes, T} ->
	        io:format("Probe should stop ~n"),
	        remove_probe(T, Nodes),
	        node(Id, Predecessor, Successor);
	 
	    {probe, Ref, Nodes, T} ->
	        forward_probe(Ref, T, Nodes, Id, Successor),
	        node(Id, Predecessor, Successor);

        {error,Error} ->
	        io:format("strange message ~w~n", [Error]),
	        node(Id, Predecessor, Successor); 
    	stop ->
        	ok
	end.

% send a request message to its successor.
stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
    	nil ->
        	Spid ! {notify,{Id, self()}},
        	{Skey, Spid};
    	{Id, _} ->
        	{Skey, Spid};
	    {Skey, _} ->
	        Spid ! {notify,{Id, self()}},
	        {Skey, Spid};
	    {Predkey, Predpid} ->
	        case key:between(Predkey, Id, Skey) of
	        true ->
	            Predpid ! {request, self()},
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

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
    nil ->
        {Nkey, Npid};
    {Pkey, _} ->
        case key:between(Nkey, Pkey, Id) of
        true ->
            {Nkey, Npid};
        false ->
            Predecessor 
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

