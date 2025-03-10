-module(concurrent_load_balancer).
-export([start/0]).

start() ->
    % Number of workers and clients
    NumWorkers = 100,
    NumClients = 30000,
    NumMessages = 100,
    BinaryMessage = <<"Hello World">>,

    Main = self(),
    io:format("Starting...~n"),

    % Spawn workers
    Workers = [spawn(fun worker/0) || _ <- lists:seq(1, NumWorkers)],

    % Spawn the load balancer
    Balancer = spawn(fun() -> load_balancer(Workers, NumClients, NumMessages) end),

    % Spawn clients
    [spawn(fun() -> client(Balancer, NumMessages, BinaryMessage, Main) end) || _ <- lists:seq(1, NumClients)],

    % Wait for all clients to finish
    lists:foreach(fun(_) ->
        receive
            ClientAnswer -> ClientAnswer
                            % , io:format("Received ~p~n", [ClientAnswer])
        end
    end, lists:seq(1, NumClients * NumMessages)),
    io:format("Stopped~n"),
    halt(0).

client(Balancer, NumMessages, Message, Main) ->
    lists:foreach(fun(_) -> Balancer ! {self(), Message} end, lists:seq(1, NumMessages)),
    lists:foreach(fun(_) ->
        receive
            Length -> Length
        end,
        Main ! Length
    end, lists:seq(1, NumMessages)).

worker() ->
    receive
        {From, Msg} ->
            Length = byte_size(Msg),
            From ! Length,
            worker();
        stop -> stop
            %io:format("Worker stopped~n")
    end.

load_balancer(Workers, NumClients, NumMessages) ->
    loop(Workers, 0, NumClients, NumMessages).

loop(Workers, Index, NumClients, NumMessages) ->
    case Index >= (NumClients * NumMessages) of
        true ->
            %io:format("Load Balancer stopped~n")
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
