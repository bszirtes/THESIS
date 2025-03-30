% ping_pong_server.erl
-module(ping_pong_server).
-export([start/0, start/1, start/2]).

start() ->
    % Register the server with a global name
    global:register_name(ping_pong_server, spawn(fun() -> server(true) end)).

start(Verbose) ->
    % Register the server with a global name
    global:register_name(ping_pong_server, spawn(fun() -> server(Verbose) end)).

start(Main, Verbose) ->
    % Register the server with a global name
    global:register_name(ping_pong_server, spawn(fun() -> server(Main, Verbose) end)).

% Server process: Handles ping messages and responds with pong messages
server(true) -> % Verbose mode enabled
    receive
        {From, ping} ->
            io:format("Server ~p received: ping~n", [self()]),
            From ! pong, % Respond with pong
            server(true); % Recursively wait for more messages
        stop ->
            io:format("Server ~p terminating.~n", [self()])
    end;
server(false) -> % Silent mode
    receive
        {From, ping} ->
            From ! pong, % Respond with pong
            server(false); % Recursively wait for more messages
        stop -> stop
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

