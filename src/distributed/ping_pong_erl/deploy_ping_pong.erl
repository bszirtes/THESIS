-module(deploy_ping_pong).
-export([main/1]).

main(Args) ->
    case parse_args(Args) of
        {ok, Nodes, NumClients, NumMessages, DelayMs, NoServer, Verbose} ->
            launch_deployment(Nodes, NumClients, NumMessages, DelayMs, NoServer, Verbose);
        {error, Msg} ->
            io:format("Error: ~s~nUsage: deploy \"[node1@host, node2@host, ...]\" <num_clients> <ping_count> <delay_ms> [no-server] [verbose]~n", [Msg])
    end.

parse_args([NodesStr, NumClientsStr, NumMessagesStr, DelayMsStr | Rest]) ->
    Nodes = parse_nodes(NodesStr),
    case {string:to_integer(NumClientsStr), string:to_integer(NumMessagesStr), string:to_integer(DelayMsStr)} of
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

launch_deployment(Nodes, NumClients, NumMessages, DelayMs, NoServer, Verbose) ->
    Main = self(), % Store reference to the main process
    ServerNode = hd(Nodes),
    start_server(ServerNode, NoServer, Main, Verbose),
    distribute_clients(Nodes, NumClients, NumMessages, DelayMs, Main, Verbose),
    io:format("All clients finished, terminating server.~n"),
    stop_server(NoServer, Verbose),
    io:format("Terminating.~n"),
    halt(0).

start_server(_, true, _, _) ->
    io:format("Skipping server start as \"no-server\" was provided.~n");
start_server(ServerNode, false, Main, Verbose) ->
    io:format("Starting server on node '~p'...~n", [ServerNode]),
    spawn(ServerNode, fun() -> ping_pong_server:start(Main, Verbose) end).

stop_server(true, _) ->
    io:format("Skipping server stop as \"no-server\" was provided.~n");
stop_server(false, true) ->
    timer:sleep(200),
    global:send(ping_pong_server, stop),
    receive
        {server, Pid, done} ->
            io:format("Server ~p finished.~n", [Pid])
    end;
stop_server(false, false) ->
    timer:sleep(200),
    global:send(ping_pong_server, stop),
    receive
        {server, _Pid, done} -> done
    end.

distribute_clients(Nodes, NumClients, NumMessages, DelayMs, Main, true) ->
    NumNodes = length(Nodes),
    io:format("Distributing ~p clients across ~p nodes to send ~p messages each...~n", [NumClients, NumNodes, NumMessages]),

    lists:foreach(fun(Index) ->
        Node = lists:nth((Index rem NumNodes) + 1, Nodes),  % Round-robin distribution
        io:format("Starting client ~p on node '~p'...~n", [Index, Node]),
        spawn(Node, fun() -> ping_pong_client:start(NumMessages, DelayMs, Main, true) end)
    end, lists:seq(1, NumClients)),

    lists:foreach(fun(_) ->
        receive
            {client, Pid, done} -> io:format("Client ~p finished.~n", [Pid])
        end
    end, lists:seq(1, NumClients));
distribute_clients(Nodes, NumClients, NumMessages, DelayMs, Main, false) ->
    NumNodes = length(Nodes),
    io:format("Distributing ~p clients across ~p nodes to send ~p messages each...~n", [NumClients, NumNodes, NumMessages]),

    lists:foreach(fun(Index) ->
        Node = lists:nth((Index rem NumNodes) + 1, Nodes),  % Round-robin distribution
        spawn(Node, fun() -> ping_pong_client:start(NumMessages, DelayMs, Main, false) end)
    end, lists:seq(1, NumClients)),

    lists:foreach(fun(_) ->
        receive
            {client, _Pid, done} -> done
        end
    end, lists:seq(1, NumClients)).

