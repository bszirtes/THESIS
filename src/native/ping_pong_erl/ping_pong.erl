%% ping_pong.erl
%%
%% This module implements a distributed message-passing system using the Actor model in Erlang.
%% It simulates clients sending messages (pings) to the server, which processes messages
%% and responds with pongs. The main process ensures orderly termination.
%%
%% Parameters:
%% - NumClients: Number of client processes.
%% - NumMessages: Number of messages each client sends.
%% - verbose: Verbose mode for debugging.
%%
%% The system terminates when all clients and the server finish processing.

-module(ping_pong).
-export([main/1]).

% Entry point of the program
main(Args) ->
    case parse_args(Args) of
        {ok, NumClients, NumMessages, Verbose} ->
            start(NumClients, NumMessages, Verbose); % Start the system with parsed parameters
        {error, Msg} ->
            io:format("Error: ~s~nUsage: main <num_clients> <num_messages> [verbose]~n", [Msg])
    end.

% Initializes the system: spawns server and clients
start(NumClients, NumMessages, Verbose) ->
    Main = self(), % Store reference to the main process

    % Spawn server processes
    io:format("Starting server...~n"),
    Server = spawn(fun() -> server(Main, Verbose) end),

    % Spawn client processes
    io:format("Starting ~p clients to send ~p messages each...~n", [NumClients, NumMessages]),
    [spawn(fun() -> client(Server, NumMessages, Main, Verbose) end) || _ <- lists:seq(1, NumClients)],

    % Wait for all clients and the server to finish
    wait_for_processes(Server, NumClients),

    io:format("Terminating.~n"),
    halt(0). % Shutdown the system

% Parses command-line arguments and converts them to integers
parse_args([NumClientsStr, NumMessagesStr | Rest]) ->
    case {string:to_integer(NumClientsStr), string:to_integer(NumMessagesStr)} of
        {{IntClients, _}, {IntMessages, _}} ->
            Verbose = lists:member("verbose", Rest), % Check for verbosity flag
            {ok, IntClients, IntMessages, Verbose};
        _ ->
            {error, "Invalid arguments."}
    end;
parse_args(_) ->
    {error, "Invalid number of arguments."}.

% Client process: Sends messages to the server and waits for responses
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

% Server process: Handles ping messages and responds with pong
server(Main, true) -> % Verbose mode enabled
    receive
        {From, ping} ->
            io:format("Server ~p received: ping~n", [self()]),
            From ! pong, % Respond with pong
            server(Main, true); % Recursively wait for more messages
        stop ->
            io:format("Server ~p terminated.~n", [self()]),
            Main ! {server, self(), done} % Notify the main process
    end;
server(Main, false) -> % Silent mode
    receive
        {From, ping} ->
            From ! pong, % Respond with pong
            server(Main, false); % Recursively wait for more messages
        stop ->
            Main ! {server, self(), done} % Notify the main process
    end.

% Waiting loop for all clients to signal completion
wait_for_processes(Server, 0) -> % Terminate when all servers and clients are done
    io:format("All clients finished, terminating server.~n"),
    Server ! stop,
    receive
        {server, _Pid, done} ->
            io:format("Server finished.~n")
    end;
wait_for_processes(Server, NumClients) ->
    receive
        {client, _Pid, done} ->
            wait_for_processes (Server, NumClients - 1) % Decrement client count
    end.

