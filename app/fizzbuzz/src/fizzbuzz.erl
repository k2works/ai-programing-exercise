-module(fizzbuzz).

-export([convert/1, convert/2, fizzbuzz_list/0, print_fizzbuzz/0]).

convert(N) ->
    convert(N, 1).

convert(N, Type) when Type =:= 1, N rem 3 =:= 0, N rem 5 =:= 0 ->
    "FizzBuzz";
convert(N, Type) when Type =:= 1, N rem 3 =:= 0 ->
    "Fizz";
convert(N, Type) when Type =:= 1, N rem 5 =:= 0 ->
    "Buzz";
convert(N, Type) when Type =:= 1 ->
    integer_to_list(N);
convert(N, Type) when Type =:= 2 ->
    integer_to_list(N);
convert(N, Type) when Type =:= 3, N rem 3 =:= 0, N rem 5 =:= 0 ->
    "FizzBuzz";
convert(N, Type) when Type =:= 3 ->
    integer_to_list(N).

fizzbuzz_list() ->
    lists:map(fun convert/1, lists:seq(1, 100)).

print_fizzbuzz() ->
    List = fizzbuzz_list(),
    lists:foreach(fun(Item) -> io:format("~s~n", [Item]) end, List),
    ok.