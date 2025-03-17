-module(ping_pong_lb).
-export([main/1]).

main(Args) ->
    case parse_args(Args) of
        {ok, NumWorkers, NumClients, NumMessages, Verbose} ->
            start(NumWorkers, NumClients, NumMessages, Verbose);
        {error, Msg} ->
            io:format("Error: ~s~nUsage: main <num_workers> <num_clients> <num_messages> [verbose]~n", [Msg])
    end.

start(NumWorkers, NumClients, NumMessages, Verbose) ->
    Main = self(),

    % Spawn watcher
    io:format("Starting watcher...~n"),
    Watcher = spawn(fun() -> watcher(Main, NumWorkers, NumClients) end),

    % Spawn workers
    io:format("Starting ~p workers...~n", [NumWorkers]),
    Workers = [spawn(fun() -> worker(Watcher, Verbose) end) || _ <- lists:seq(1, NumWorkers)],

    % Spawn the load balancer
    io:format("Starting load balancer...~n"),
    Balancer = spawn(fun() -> load_balancer(Workers, NumClients, NumMessages) end),

    % Spawn clients
    io:format("Starting ~p clients to send ~p messages each...~n", [NumClients, NumMessages]),
    [spawn(fun() -> client(Balancer, NumMessages, Watcher, Verbose) end) || _ <- lists:seq(1, NumClients)],

    % Wait for all clients and workers to finish
    receive
        done ->
            io:format("Terminating.~n")
    end,
    halt(0).

parse_args([NumWorkersStr, NumClientsStr, NumMessagesStr | Rest]) ->
    case {string:to_integer(NumWorkersStr), string:to_integer(NumClientsStr), string:to_integer(NumMessagesStr)} of
        {{IntWorkers, _}, {IntClients, _}, {IntMessages, _}} ->
            Verbose = lists:member("verbose", Rest),
            {ok, IntWorkers, IntClients, IntMessages, Verbose};
        _ ->
            {error, "Invalid arguments."}
    end;
parse_args(_) ->
    {error, "Invalid number of arguments."}.

client(Balancer, NumMessages, Main, true) ->
    lists:foreach(fun(_) -> Balancer ! {self(), ping} end, lists:seq(1, NumMessages)),
    lists:foreach(fun(_) ->
        receive
            pong ->
                io:format("Client ~p received: pong~n", [self()])
        end
    end, lists:seq(1, NumMessages)),
    io:format("Client ~p reached max pings, terminating.~n", [self()]),
    Main ! {client, self(), done};
client(Balancer, NumMessages, Main, false) ->
    lists:foreach(fun(_) -> Balancer ! {self(), ping} end, lists:seq(1, NumMessages)),
    lists:foreach(fun(_) ->
        receive
            pong -> pong
        end
    end, lists:seq(1, NumMessages)),
    Main ! {client, self(), done}.

worker(Main, true) ->
    receive
        {From, ping} ->
            io:format("Worker ~p received: ping~n", [self()]),
            From ! pong,
            worker(Main, true);
        stop ->
            io:format("Worker ~p terminated.~n", [self()]),
            Main ! {worker, self(), done}
    end;
worker(Main, false) ->
    receive
        {From, ping} ->
            From ! pong,
            worker(Main, false);
        stop ->
            Main ! {worker, self(), done}
    end.

load_balancer(Workers, NumClients, NumMessages) ->
    loop(Workers, 0, NumClients, NumMessages).

loop(Workers, Index, NumClients, NumMessages) ->
    case Index >= (NumClients * NumMessages) of
        true ->
            io:format("Load balancer ~p finished, stopping all workers.~n", [self()]),
            lists:foreach(fun(Worker) -> Worker ! stop end, Workers);
        _ ->
            receive
                {From, Msg} ->
                    % Route the message to a worker in round-robin fashion
                    Worker = lists:nth((Index rem length(Workers)) + 1, Workers),
                    Worker ! {From, Msg},
                    loop(Workers, (Index + 1), NumClients, NumMessages)
            end
    end.

watcher(Main, 0, 0) ->
    io:format("All clients and workers finished.~n"),
    Main ! done;
watcher(Main, NumWorkers, NumClients) ->
    receive
        {worker, _Pid, done} -> 
            watcher(Main, NumWorkers - 1, NumClients);
        {client, _Pid, done} ->
            watcher(Main, NumWorkers, NumClients - 1)
    end.

