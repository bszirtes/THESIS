%% ping_pong_lb.erl
%%
%% This module implements a distributed message-passing system using the Actor model in Erlang.
%% It simulates clients sending messages (pings) to workers through a load balancer.
%% Workers process messages and respond with pongs. The main process ensures orderly termination.
%%
%% Parameters:
%% - NumWorkers: Number of worker processes.
%% - NumClients: Number of client processes.
%% - NumMessages: Number of messages each client sends.
%% - verbose: Verbose mode for debugging.
%%
%% The system terminates when all clients and workers finish processing.

-module(ping_pong_lb).
-export([main/1]).

% Entry point of the program
main(Args) ->
    case parse_args(Args) of
        {ok, NumWorkers, NumClients, NumMessages, Verbose} ->
            start(NumWorkers, NumClients, NumMessages, Verbose); % Start the system with parsed parameters
        {error, Msg} ->
            io:format("Error: ~s~nUsage: main <num_workers> <num_clients> <num_messages> [verbose]~n", [Msg])
    end.

% Initializes the system: spawns workers, clients and load balancer
start(NumWorkers, NumClients, NumMessages, Verbose) ->
    Main = self(), % Store reference to the main process

    % Spawn worker processes
    io:format("Starting ~p workers...~n", [NumWorkers]),
    Workers = [spawn(fun() -> worker(Main, Verbose) end) || _ <- lists:seq(1, NumWorkers)],

    % Spawn load balancer to distribute tasks among workers
    io:format("Starting load balancer...~n"),
    Balancer = spawn(fun() -> load_balancer(Workers, NumClients, NumMessages) end),

    % Spawn client processes
    io:format("Starting ~p clients to send ~p messages each...~n", [NumClients, NumMessages]),
    [spawn(fun() -> client(Balancer, NumMessages, Main, Verbose) end) || _ <- lists:seq(1, NumClients)],

    % Wait for all clients and workers to finish
    wait_for_processes(NumWorkers, NumClients),

    io:format("Terminating.~n"),
    halt(0). % Shutdown the system

% Parses command-line arguments and converts them to integers
parse_args([NumWorkersStr, NumClientsStr, NumMessagesStr | Rest]) ->
    case {string:to_integer(NumWorkersStr), string:to_integer(NumClientsStr), string:to_integer(NumMessagesStr)} of
        {{IntWorkers, _}, {IntClients, _}, {IntMessages, _}} ->
            Verbose = lists:member("verbose", Rest), % Check for verbosity flag
            {ok, IntWorkers, IntClients, IntMessages, Verbose};
        _ ->
            {error, "Invalid arguments."}
    end;
parse_args(_) ->
    {error, "Invalid number of arguments."}.

% Client process: Sends messages to the balancer and waits for responses
client(Balancer, NumMessages, Main, true) -> % Verbose mode enabled
    lists:foreach(fun(_) -> Balancer ! {self(), ping} end, lists:seq(1, NumMessages)), % Send pings
    lists:foreach(fun(_) ->
        receive
            pong -> io:format("Client ~p received: pong~n", [self()])
        end
    end, lists:seq(1, NumMessages)),
    io:format("Client ~p reached max pings, terminating.~n", [self()]),
    Main ! {client, self(), done}; % Notify the main process that client is done
client(Balancer, NumMessages, Main, false) -> % Silent mode
    lists:foreach(fun(_) -> Balancer ! {self(), ping} end, lists:seq(1, NumMessages)), % Send pings
    lists:foreach(fun(_) ->
        receive
            pong -> pong % Discard received message
        end
    end, lists:seq(1, NumMessages)),
    Main ! {client, self(), done}. % Notify the main process

% Worker process: Handles ping messages and responds with pong
worker(Main, true) -> % Verbose mode enabled
    receive
        {From, ping} ->
            io:format("Worker ~p received: ping~n", [self()]),
            From ! pong, % Respond with pong
            worker(Main, true); % Recursively wait for more messages
        stop ->
            io:format("Worker ~p terminated.~n", [self()]),
            Main ! {worker, self(), done} % Notify the main process
    end;
worker(Main, false) -> % Silent mode
    receive
        {From, ping} ->
            From ! pong, % Respond with pong
            worker(Main, false); % Recursively wait for more messages
        stop ->
            Main ! {worker, self(), done} % Notify the main process
    end.

% Load balancer: Distributes messages among workers using round-robin
load_balancer(Workers, NumClients, NumMessages) ->
    loop(Workers, 0, NumClients, NumMessages).

% Core loop of the load balancer
loop(Workers, Index, NumClients, NumMessages) ->
    case Index >= (NumClients * NumMessages) of % Check if all messages are processed
        true ->
            io:format("Load balancer ~p finished, stopping all workers.~n", [self()]),
            lists:foreach(fun(Worker) -> Worker ! stop end, Workers); % Stop workers
        _ ->
            receive
                {From, Msg} ->
                    % Select a worker using round-robin scheduling
                    Worker = lists:nth((Index rem length(Workers)) + 1, Workers),
                    Worker ! {From, Msg}, % Forward message to worker
                    loop(Workers, (Index + 1), NumClients, NumMessages) % Continue loop
            end
    end.

% Waiting loop for all workers and clients to signal completion
wait_for_processes(0, 0) -> % Terminate when all workers and clients are done
    io:format("All clients and workers finished.~n");
wait_for_processes(NumWorkers, NumClients) ->
    receive
        {worker, _Pid, done} ->
            wait_for_processes (NumWorkers - 1, NumClients); % Decrement worker count
        {client, _Pid, done} ->
            wait_for_processes (NumWorkers, NumClients - 1) % Decrement client count
    end.

