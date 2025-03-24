% ping_pong_client.erl
-module(ping_pong_client).
-export([start/1, start/2, start/3, start/5]).

start(ServerNode) ->
    % Start the ping-pong loop with the given server's fully-qualified node name
    ping_pong(ServerNode, 10, 1000).

start(ServerNode, MaxPings) ->
    % Start the ping-pong loop with the given server's fully-qualified node name
    ping_pong(ServerNode, MaxPings, 1000).

start(ServerNode, MaxPings, DelayMs) ->
    % Start the ping-pong loop with the given server's fully-qualified node name
    ping_pong(ServerNode, MaxPings, DelayMs).

start(ServerNode, MaxPings, DelayMs, Handler, Verbose) ->
    % Start the ping-pong loop with the given server's fully-qualified node name by a handler process
    ping_pong(ServerNode, MaxPings, DelayMs, Handler, Verbose).

ping_pong(_, 0, _) ->
    io:format("Client reached max pings, terminating.~n");
ping_pong(ServerNode, MaxPings, DelayMs) ->
    % Send a ping to the server
    global:send(ping_pong_server, {ping, self()}),

    % Wait for a pong message
    receive
        pong ->
            io:format("Client ~p received: pong~n", [self()]),
            % Delay before sending the next ping
            timer:sleep(DelayMs), % Delay milliseconds
            ping_pong(ServerNode, MaxPings - 1, DelayMs)
    end.

ping_pong(_, 0, _, Handler, _) ->
    % Signal completion to handler process
    Handler ! {self(), done},
    io:format("Client ~p reached max pings, terminating.~n", [self()]);
% Verbose output
ping_pong(ServerNode, MaxPings, DelayMs, Handler, true) ->
    % Send a ping to the server
    global:send(ping_pong_server, {ping, self()}),

    % Wait for a pong message
    receive
        pong ->
            io:format("Client ~p received: pong~n", [self()]),
            % Delay before sending the next ping
            timer:sleep(DelayMs), % Delay milliseconds
            ping_pong(ServerNode, MaxPings - 1, DelayMs, Handler, true)
    end;
ping_pong(ServerNode, MaxPings, DelayMs, Handler, false) ->
    % Send a ping to the server
    global:send(ping_pong_server, {ping, self()}),

    % Wait for a pong message
    receive
        pong ->
            % Delay before sending the next ping
            timer:sleep(DelayMs), % Delay milliseconds
            ping_pong(ServerNode, MaxPings - 1, DelayMs, Handler, false)
    end.

