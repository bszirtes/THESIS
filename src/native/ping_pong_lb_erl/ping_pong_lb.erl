%% ping_pong_lb.erl
%%
%% This module implements a distributed message-passing system using the Actor model in Erlang.
%% It simulates clients sending messages (pings) to servers through a load balancer.
%% Servers process messages and respond with pongs. The main process ensures orderly termination.
%%
%% Parameters:
%% - NumServers: Number of server processes.
%% - NumClients: Number of client processes.
%% - NumMessages: Number of messages each client sends.
%% - verbose: Verbose mode for debugging.
%%
%% The system terminates when the clients, the servers and the load balancer finish processing.

-module(ping_pong_lb).
-export([main/0, main/1]).

% Entry point of the program
main() ->
    start(2, 8, 10, false).

main(Args) ->
    case parse_args(Args) of
        {ok, NumServers, NumClients, NumMessages, Verbose} ->
            start(NumServers, NumClients, NumMessages, Verbose); % Start the system with parsed parameters
        {error, Msg} ->
            io:format("Error: ~s~nUsage: main <num_servers> <num_clients> <num_messages> [verbose]~n", [Msg])
    end.

% Initializes the system: spawns servers, clients and load balancer
start(NumServers, NumClients, NumMessages, Verbose) ->
    Main = self(), % Store reference to the main process

    % Spawn server processes
    io:format("Starting ~p servers...~n", [NumServers]),
    Servers = [spawn(fun() -> server(Main, Verbose) end) || _ <- lists:seq(1, NumServers)],

    % Spawn load balancer process
    io:format("Starting load balancer...~n"),
    LoadBalancer = spawn(fun() -> load_balancer(Servers, 0, Main, Verbose) end),

    % Spawn client processes
    io:format("Starting ~p clients to send ~p messages each...~n", [NumClients, NumMessages]),
    [spawn(fun() -> client(LoadBalancer, NumMessages, Main, Verbose) end) || _ <- lists:seq(1, NumClients)],

    % Wait for all clients, servers and the load balancer to finish
    wait_for_processes(LoadBalancer, Servers, NumServers, NumClients, Verbose),

    io:format("Terminating.~n"),
    halt(0). % Shutdown the system

% Parses command-line arguments and converts them
parse_args([NumServersStr, NumClientsStr, NumMessagesStr | Rest]) ->
    case {string:to_integer(NumServersStr), string:to_integer(NumClientsStr), string:to_integer(NumMessagesStr)} of
        {{IntServer, _}, {IntClients, _}, {IntMessages, _}} ->
            Verbose = lists:member("verbose", Rest), % Check for verbosity flag
            {ok, IntServer, IntClients, IntMessages, Verbose};
        _ ->
            {error, "Invalid arguments."}
    end;
parse_args(_) ->
    {error, "Invalid number of arguments."}.


% Client process: Sends messages to the load balancer and waits for responses
client(_, 0, Main, true) ->
    io:format("Client ~p reached max pings, terminating.~n", [self()]),
    Main ! {client, self(), done}; % Notify the main process that client is done
client(_, 0, Main, false) ->
    Main ! {client, self(), done}; % Notify the main process that client is done
client(LoadBalancer, NumMessages, Main, true) -> % Verbose mode
    % Send a ping to the server
    LoadBalancer ! {self(), ping},

    % Wait for a pong message
    receive
        pong ->
            io:format("Client ~p received: pong~n", [self()]),
            client(LoadBalancer, NumMessages - 1, Main, true) % Recursively send more messages
    end;
client(LoadBalancer, NumMessages, Main, false) -> % Silent mode
    % Send a ping to the server
    LoadBalancer ! {self(), ping},

    % Wait for a pong message
    receive
        pong ->
            client(LoadBalancer, NumMessages - 1, Main, false) % Recursively send more messages
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

% Load balancer: Distributes messages among servers in a round-robin manner
load_balancer(Servers, Index, Main, true) ->
    receive
        {From, Msg} ->
            % Select a worker using round-robin scheduling
            Server = lists:nth((Index rem length(Servers)) + 1, Servers),
            Server ! {From, Msg}, % Forward message to worker
            load_balancer(Servers, (Index + 1), Main, true); % Continue loop
        stop ->
            io:format("Load balancer ~p terminating.~n", [self()]),
            Main ! {lb, self(), done} % Notify the main process
    end;
load_balancer(Servers, Index, Main, false) ->
    receive
        {From, Msg} ->
            % Select a worker using round-robin scheduling
            Server = lists:nth((Index rem length(Servers)) + 1, Servers),
            Server ! {From, Msg}, % Forward message to worker
            load_balancer(Servers, (Index + 1), Main, false); % Continue loop
        stop ->
            Main ! {lb, self(), done} % Notify the main process
    end.

% Waiting loop for all clients to signal completion
wait_for_processes(LoadBalancer, _, 0, (-1), true) -> % Terminate load balancer when servers are done
    io:format("All servers finished, terminating load balancer.~n"),
    LoadBalancer ! stop,
    receive
        {lb, Pid, done} ->
            io:format("Load balancer ~p finished.~n", [Pid])
    end;
wait_for_processes(LoadBalancer, _, 0, (-1), false) -> % Terminate load balancer when servers are done
    io:format("All servers finished, terminating load balancer.~n"),
    LoadBalancer ! stop,
    receive
        {lb, _Pid, done} -> done
    end;
wait_for_processes(LoadBalancer, Servers, NumServers, (-1), true) ->
    receive
        {server, Pid, done} ->
            io:format("Server ~p finished.~n", [Pid]),
            wait_for_processes(LoadBalancer, Servers, NumServers - 1, (-1), true) % Decrement server count
    end;
wait_for_processes(LoadBalancer, Servers, NumServers, (-1), false) ->
    receive
        {server, _Pid, done} ->
            wait_for_processes(LoadBalancer, Servers, NumServers - 1, (-1), false) % Decrement server count
    end;
wait_for_processes(LoadBalancer, Servers, NumServers, 0, Verbose) -> % Terminate servers when clients are done
    io:format("All clients finished, terminating servers.~n"),
    lists:foreach(fun(Server) -> Server ! stop end, Servers), % Stop servers
    wait_for_processes(LoadBalancer, Servers, NumServers, (-1), Verbose);
wait_for_processes(LoadBalancer, Servers, NumServers, NumClients, true) ->
    receive
        {client, Pid, done} ->
            io:format("Client ~p finished.~n", [Pid]),
            wait_for_processes(LoadBalancer, Servers, NumServers, NumClients - 1, true) % Decrement client count
    end;
wait_for_processes(LoadBalancer, Servers, NumServers, NumClients, false) ->
    receive
        {client, _Pid, done} ->
            wait_for_processes(LoadBalancer, Servers, NumServers, NumClients - 1, false) % Decrement client count
    end.

