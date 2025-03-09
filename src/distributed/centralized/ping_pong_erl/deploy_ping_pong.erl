-module(deploy_ping_pong).
-export([deploy/1]).

deploy(Args) ->
    io:format("Args: ~p\n", [Args]),
    case parse_args(Args) of
        {ok, NumClients, PingCount, DelayMs, NoServer, Verbose} ->
            launch_deployment(NumClients, PingCount, DelayMs, NoServer, Verbose);
        {error, Msg} ->
            io:format("Error: ~s~nUsage: deploy <num_clients> <ping_count> <delay_ms> [no-server] [verbose]~n", [Msg])
    end.

parse_args([NumClientsStr, PingCountStr, DelayMsStr | Rest]) ->
    case {string:to_integer(NumClientsStr), string:to_integer(PingCountStr), string:to_integer(DelayMsStr)} of
        {{IntClients, _}, {IntPings, _}, {IntDelay, _}} ->
            NoServer = lists:member("no-server", Rest),
            Verbose = lists:member("verbose", Rest),
            {ok, IntClients, IntPings, IntDelay, NoServer, Verbose};
        _ ->
            {error, "Invalid arguments."}
    end;
parse_args(_) ->
    {error, "Invalid number of arguments."}.

launch_deployment(NumClients, PingCount, DelayMs, NoServer, Verbose) ->
    Nodes = ['server@localhost', 'client@localhost'],
    Handler = self(),
    start_server(hd(Nodes), NoServer, Verbose),
    io:format("~p ~p ~p~n", [NumClients, NoServer, Verbose]),
    spawn('client@localhost', fun() -> ping_pong_client:start('server@localhost', PingCount, DelayMs, Handler, Verbose) end),
    receive
        {Pid, done} -> io:format("Client ~p finished.~n", [Pid])
    end,
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

