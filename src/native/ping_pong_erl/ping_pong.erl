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
-export([main/0, main/1]).

% Entry point of the program
main() ->
    start(4, 10, false).

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
    wait_for_processes(Server, NumClients, Verbose),

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
client(_, 0, Main, true) ->
    io:format("Client ~p reached max pings, terminating.~n", [self()]),
    Main ! {client, self(), done}; % Notify the main process that client is done
client(_, 0, Main, false) ->
    Main ! {client, self(), done}; % Notify the main process that client is done
client(Server, NumMessages, Main, true) -> % Verbose mode
    % Send a ping to the server
    Server ! {self(), ping},

    % Wait for a pong message
    receive
        pong ->
            io:format("Client ~p received: pong~n", [self()]),
            client(Server, NumMessages - 1, Main, true) % Recursively send more messages
    end;
client(Server, NumMessages, Main, false) -> % Silent mode
    % Send a ping to the server
    Server ! {self(), ping},

    % Wait for a pong message
    receive
        pong ->
            client(Server, NumMessages - 1, Main, false) % Recursively send more messages
    end.

% Server process: Handles ping messages and responds with pong messages
server(Main, true) -> % Verbose mode enabled
    receive
        {From, ping} ->
            io:format("Server ~p received: ping~n", [self()]),
            From ! pong, % Respond with pong
            server(Main, true); % Recursively wait for more messages
        stop ->
            io:format("Server ~p terminating.~n", [self()]),
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

% Waiting loop for all clients and the server to signal completion
wait_for_processes(Server, 0, true) -> % Terminate server when clients are done
    io:format("All clients finished, terminating server.~n"),
    Server ! stop,
    receive
        {server, Pid, done} ->
            io:format("Server ~p finished.~n", [Pid])
    end;
wait_for_processes(Server, 0, false) -> % Terminate server when clients are done
    io:format("All clients finished, terminating server.~n"),
    Server ! stop,
    receive
        {server, _Pid, done} -> done
    end;
wait_for_processes(Server, NumClients, true) ->
    receive
        {client, Pid, done} ->
            io:format("Client ~p finished.~n", [Pid]),
            wait_for_processes(Server, NumClients - 1, true) % Decrement client count
    end;
wait_for_processes(Server, NumClients, false) ->
    receive
        {client, _Pid, done} ->
            wait_for_processes(Server, NumClients - 1, false) % Decrement client count
    end.

