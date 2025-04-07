% ping_pong_lb_prime_server.erl
-module(ping_pong_lb_prime_server).
-export([start/0, start/1, start/2]).

start() ->
    % Connect to load balancer
    global:send(ping_pong_load_balancer, {self(), register}),
    server(true).

start(Verbose) ->
    % Connect to load balancer
    global:send(ping_pong_load_balancer, {self(), register}),
    server(Verbose).

start(Main, Verbose) ->
    % Connect to load balancer
    global:send(ping_pong_load_balancer, {self(), register}),
    server(Main, Verbose).

% Server process: Handles messages from clients and responds with the found number of primes in the received range
server(true) -> % Verbose mode enabled
    receive
        {From, PrimeRange} ->
            io:format("Server ~p received: ~p~n", [self(), PrimeRange]),
            From ! find_primes(PrimeRange), % Respond with the number of primes found
            server(true); % Recursively wait for more messages
        stop ->
            io:format("Server ~p terminating.~n", [self()])
    end;
server(false) -> % Silent mode
    receive
        {From, PrimeRange} ->
            From ! find_primes(PrimeRange), % Respond with the number of primes found
            server(false); % Recursively wait for more messages
        stop -> stop
    end.

% Server process: Handles messages from clients and responds with the found number of primes in the received range
server(Main, true) -> % Verbose mode enabled
    receive
        {From, PrimeRange} ->
            io:format("Server ~p received: ~p~n", [self(), PrimeRange]),
            From ! find_primes(PrimeRange), % Respond with the number of primes found
            server(Main, true); % Recursively wait for more messages
        stop ->
            io:format("Server ~p terminating.~n", [self()]),
            Main ! {server, self(), done} % Notify the main process
    end;
server(Main, false) -> % Silent mode
    receive
        {From, PrimeRange} ->
            From ! find_primes(PrimeRange), % Respond with the number of primes found
            server(Main, false); % Recursively wait for more messages
        stop ->
            Main ! {server, self(), done} % Notify the main process
    end.

%% Function to check if a number is prime
is_prime(N) when N < 2 -> false;
is_prime(2) -> true;
is_prime(N) ->
    Max = trunc(math:sqrt(N)),
    not lists:any(fun(X) -> N rem X =:= 0 end, lists:seq(2, Max)).

%% Function to count prime numbers up to a given limit
find_primes(Max) ->
    lists:foldl(fun(N, Acc) ->
                    case is_prime(N) of
                        true -> Acc + 1;
                        false -> Acc
                    end
                end, 0, lists:seq(2, Max)).

