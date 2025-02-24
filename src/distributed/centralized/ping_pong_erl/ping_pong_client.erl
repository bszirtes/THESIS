% ping_pong_client.erl
-module(ping_pong_client).
-export([start/1, start/2, start/3]).

start(ServerNode) ->
    % Start the ping-pong loop with the given server's fully-qualified node name
    ping_pong(ServerNode, 10, 1000).

start(ServerNode, MaxPings) ->
    % Start the ping-pong loop with the given server's fully-qualified node name
    ping_pong(ServerNode, MaxPings, 1000).

start(ServerNode, MaxPings, DelayMs) ->
    % Start the ping-pong loop with the given server's fully-qualified node name
    ping_pong(ServerNode, MaxPings, DelayMs).

ping_pong(_, 0, _) ->
    io:format("Client reached max pings, terminating.~n");
ping_pong(ServerNode, MaxPings, DelayMs) ->
    % Send a ping to the server
    global:send(ping_pong_server, {ping, self()}),

    % Wait for a pong message
    receive
        pong ->
            io:format("Client received: pong~n"),
            % Delay before sending the next ping
            timer:sleep(DelayMs), % Delay milliseconds
            ping_pong(ServerNode, MaxPings - 1, DelayMs)
    end.

