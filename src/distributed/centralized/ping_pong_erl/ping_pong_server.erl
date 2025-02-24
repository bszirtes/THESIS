% ping_pong_server.erl
-module(ping_pong_server).
-export([start/0]).

start() ->
    % Start the server with a unique process identifier
    global:register_name(ping_pong_server, spawn(fun() -> loop() end)).

loop() ->
    % The server will wait for messages and handle them accordingly
    receive
        {ping, Sender} ->
            io:format("Server received: ping~n"),
            Sender ! pong,  % Reply with pong
            loop()
    end.

