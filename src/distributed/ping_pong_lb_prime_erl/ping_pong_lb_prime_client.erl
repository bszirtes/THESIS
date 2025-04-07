% ping_pong_lb_prime_client.erl
-module(ping_pong_lb_prime_client).
-export([start/0, start/1, start/2, start/4]).

start() ->
    client(10, 50000).

start(NumMessages) ->
    client(NumMessages, 50000).

start(NumMessages, PrimeRange) ->
    client(NumMessages, PrimeRange).

start(NumMessages, PrimeRange, Main, Verbose) ->
    client(NumMessages, PrimeRange, Main, Verbose).

% Client process: Sends messages to the load balancer and waits for the number of the found primes in the sent range
client(0, _) ->
    io:format("Client ~p reached max messages, terminating.~n", [self()]);
client(NumMessages, PrimeRange) ->
    % Send the range to the load balancer
    global:send(ping_pong_load_balancer, {self(), PrimeRange}),

    % Wait for the found primes
    receive
        FoundPrimes ->
            io:format("Client ~p received: ~p~n", [self(), FoundPrimes]),
            client(NumMessages - 1, PrimeRange) % Recursively send more messages
    end.

% Client process: Sends messages to the load balancer and waits for the number of the found primes in the sent range
client(0, _, Main, true) ->
    io:format("Client ~p reached max messages, terminating.~n", [self()]),
    Main ! {client, self(), done}; % Notify the main process that client is done
client(0, _, Main, false) ->
    Main ! {client, self(), done}; % Notify the main process that client is done
client(NumMessages, PrimeRange, Main, true) -> % Verbose mode
    % Send the range to the load balancer
    global:send(ping_pong_load_balancer, {self(), PrimeRange}),

    % Wait for the found primes
    receive
        FoundPrimes ->
            io:format("Client ~p received: ~p~n", [self(), FoundPrimes]),
            client(NumMessages - 1, PrimeRange, Main, true) % Recursively send more messages
    end;
client(NumMessages, PrimeRange, Main, false) -> % Silent mode
    % Send the range to the load balancer
    global:send(ping_pong_load_balancer, {self(), PrimeRange}),

    % Wait for the found primes
    receive
        _FoundPrimes ->
            client(NumMessages - 1, PrimeRange, Main, false) % Recursively send more messages
    end.

