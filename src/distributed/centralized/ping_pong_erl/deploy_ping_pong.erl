-module(deploy_ping_pong).
-export([deploy/1]).

deploy(Args) ->
    case parse_args(Args) of
        {ok, Nodes, NumClients, PingCount, DelayMs, NoServer, Verbose} ->
            launch_deployment(Nodes, NumClients, PingCount, DelayMs, NoServer, Verbose);
        {error, Msg} ->
            io:format("Error: ~s~nUsage: deploy \"[node1@host, node2@host, ...]\" <num_clients> <ping_count> <delay_ms> [--no-server]~n", [Msg])
    end.

parse_args([NodesStr, NumClientsStr, PingCountStr, DelayMsStr | Rest]) ->
    Nodes = parse_nodes(NodesStr),
    case {string:to_integer(NumClientsStr), string:to_integer(PingCountStr), string:to_integer(DelayMsStr)} of
        {{IntClients, _}, {IntPings, _}, {IntDelay, _}} ->
            NoServer = lists:member("no-server", Rest),
            Verbose = lists:member("verbose", Rest),
            {ok, Nodes, IntClients, IntPings, IntDelay, NoServer, Verbose};
        _ ->
            {error, "Invalid arguments."}
    end;
parse_args(_) ->
    {error, "Invalid number of arguments."}.

parse_nodes(NodesStr) ->
    Stripped = string:trim(NodesStr, both, "[]"),
    Parts = string:split(Stripped, ",", all),
    [list_to_atom(string:trim(P)) || P <- Parts].

launch_deployment(Nodes, NumClients, PingCount, DelayMs, NoServer, Verbose) ->
    ServerNode = hd(Nodes),
    start_server(ServerNode, NoServer, Verbose),
    distribute_clients(Nodes, NumClients, PingCount, DelayMs, Verbose, ServerNode),
    io:format("All clients finished, terminating.~n"),
    stop_server(NoServer).

start_server(_, true, _) ->
    io:format("Skipping server start as \"no-server\" was provided.~n");
start_server(ServerNode, false, Verbose) ->
    io:format("Starting server on node '~p'...~n", [ServerNode]),
    spawn(ServerNode, fun() -> ping_pong_server:start(Verbose) end).

stop_server(true) ->
    io:format("Skipping server stop as \"no-server\" was provided.~n");
stop_server(_) ->
    io:format("Stopping server...~n"),
    global:send(ping_pong_server, stop).

distribute_clients(Nodes, NumClients, PingCount, DelayMs, Verbose, ServerNode) ->
    Handler = self(),
    NumNodes = length(Nodes),
    io:format("Distributing ~p clients across ~p nodes (including server node).~n", [NumClients, NumNodes]),

    lists:foreach(fun(Index) ->
        Node = lists:nth((Index rem NumNodes) + 1, Nodes),  % Round-robin distribution
        io:format("Starting client ~p on node ~s...~n", [Index, Node]),
        spawn(Node, fun() -> ping_pong_client:start(ServerNode, PingCount, DelayMs, Handler, Verbose) end)
    end, lists:seq(1, NumClients)),

    io:format("All clients started.~n"),

    lists:foreach(fun(_) ->
        receive
            {Pid, done} -> io:format("Client ~p finished.~n", [Pid])
        end
    end, lists:seq(1, NumClients)).

