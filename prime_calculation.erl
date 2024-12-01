-module(prime_calculation).
-export([main/1]).

is_prime(N) when N < 2 -> false;
is_prime(2) -> true;
is_prime(N) ->
    Max = trunc(math:sqrt(N)),
    not lists:any(fun(X) -> N rem X =:= 0 end, lists:seq(2, Max)).

find_primes(Max) ->
    lists:foldl(fun(N, Acc) -> 
                    case is_prime(N) of
                        true -> Acc + 1;
                        false -> Acc
                    end
                end, 0, lists:seq(2, Max)).

main(Args) ->
    Range = list_to_integer(hd(Args)),
    Start = erlang:monotonic_time(),
    Count = find_primes(Range),
    End = erlang:monotonic_time(),
    Elapsed = erlang:convert_time_unit(End - Start, native, second),
    io:format("Found ~p prime numbers in range 1 to ~p~n", [Count, Range]),
    io:format("Time elapsed: ~p seconds.~n", [Elapsed]),
    halt(0).
