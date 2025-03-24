% ping_pong_server.erl
-module(ping_pong_server).
-export([start/0, start/1]).

start() ->
    % Register the server with a global name
    global:register_name(ping_pong_server, spawn(fun() -> loop(true) end)),
    self().

start(Verbose) ->
    % Register the server with a global name
    global:register_name(ping_pong_server, spawn(fun() -> loop(Verbose) end)),
    self().

% Verbose output
loop(true) ->
    % The server will wait for messages and handle them accordingly
    receive
        {ping, Sender} ->
            io:format("Server ~p received: ping~n", [self()]),
            Sender ! pong,  % Reply with pong
            loop(true);
        stop ->
            io:format("Server ~p terminated.~n", [self()])
    end;
loop(false) ->
    % The server will wait for messages and handle them accordingly
    receive
        {ping, Sender} ->
            Sender ! pong,  % Reply with pong
            loop(false);
        stop ->
            io:format("Server ~p terminated.~n", [self()])
    end.

