%% prime_calculation.erl
%%
%% Module for calculating prime numbers in a given range.

-module(prime_calculation).
-export([main/1]).

%% Function to check if a number is prime
is_prime(N) when N < 2 -> false;  % Numbers less than 2 are not prime
is_prime(2) -> true;              % 2 is the only even prime number
is_prime(N) ->
    Max = trunc(math:sqrt(N)),    % Determine upper limit for divisibility check
    not lists:any(fun(X) -> N rem X =:= 0 end, lists:seq(2, Max)).
    % Check if any number from 2 to Max divides N without remainder

%% Function to count prime numbers up to a given limit
find_primes(Max) ->
    lists:foldl(fun(N, Acc) ->    % Iterate through numbers and count primes
                    case is_prime(N) of
                        true -> Acc + 1;  % Increment count if N is prime
                        false -> Acc      % Otherwise, keep count unchanged
                    end
                end, 0, lists:seq(2, Max)).

%% Main function to execute prime calculation
main(Args) ->
    Range = list_to_integer(hd(Args)),  % Convert input argument to integer
    Start = erlang:monotonic_time(),    % Get start time in native units
    Count = find_primes(Range),         % Count prime numbers in the given range
    End = erlang:monotonic_time(),      % Get end time
    Elapsed = erlang:convert_time_unit(End - Start, native, second),
    % Convert elapsed time to seconds

    %% Print the results
    io:format("Found ~p prime numbers in range 1 to ~p~n", [Count, Range]),
    io:format("Time elapsed: ~p seconds.~n", [Elapsed]),

    halt(0).  % Terminate program execution

