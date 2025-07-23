-module(fizzbuzz).

-export([convert/1, fizzbuzz_list/0, print_fizzbuzz/0]).

convert(N) when N rem 3 =:= 0, N rem 5 =:= 0 ->
    "FizzBuzz";
convert(N) when N rem 3 =:= 0 ->
    "Fizz";
convert(N) when N rem 5 =:= 0 ->
    "Buzz";
convert(N) ->
    integer_to_list(N).

fizzbuzz_list() ->
    lists:map(fun convert/1, lists:seq(1, 100)).

print_fizzbuzz() ->
    List = fizzbuzz_list(),
    lists:foreach(fun(Item) -> io:format("~s~n", [Item]) end, List),
    ok.