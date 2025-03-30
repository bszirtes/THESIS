% ping_pong_client.erl
-module(ping_pong_client).
-export([start/0, start/1, start/2, start/4]).

start() ->
    client(10, 1000).

start(NumMessages) ->
    client(NumMessages, 1000).

start(NumMessages, DelayMs) ->
    client(NumMessages, DelayMs).

start(NumMessages, DelayMs, Main, Verbose) ->
    client(NumMessages, DelayMs, Main, Verbose).

% Client process: Sends messages to the server and waits for responses
client(0, _) ->
    io:format("Client ~p reached max pings, terminating.~n", [self()]);
client(NumMessages, DelayMs) ->
    % Send a ping to the server
    global:send(ping_pong_server, {self(), ping}),

    % Wait for a pong message
    receive
        pong ->
            io:format("Client ~p received: pong~n", [self()]),
            % Delay before sending the next ping
            timer:sleep(DelayMs), % Delay milliseconds
            client(NumMessages - 1, DelayMs)
    end.

% Client process: Sends messages to the server and waits for responses
client(0, _, Main, true) ->
    io:format("Client ~p reached max pings, terminating.~n", [self()]),
    Main ! {client, self(), done}; % Notify the main process that client is done
client(0, _, Main, false) ->
    Main ! {client, self(), done}; % Notify the main process that client is done
client(NumMessages, DelayMs, Main, true) ->
    % Send a ping to the server
    global:send(ping_pong_server, {self(), ping}),

    % Wait for a pong message
    receive
        pong ->
            io:format("Client ~p received: pong~n", [self()]),
            % Delay before sending the next ping
            timer:sleep(DelayMs), % Delay milliseconds
            client(NumMessages - 1, DelayMs, Main, true)
    end;
client(NumMessages, DelayMs, Main, false) -> % Silent mode
    % Send a ping to the server
    global:send(ping_pong_server, {self(), ping}),

    % Wait for a pong message
    receive
        pong ->
            % Delay before sending the next ping
            timer:sleep(DelayMs), % Delay milliseconds
            client(NumMessages - 1, DelayMs, Main, false)
    end.

