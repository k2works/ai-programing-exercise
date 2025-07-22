-module(fizzbuzz).
-export([convert/1, range/2, fizzbuzz_list/2]).

convert(N) when N rem 15 =:= 0 ->
    "FizzBuzz";
convert(N) when N rem 3 =:= 0 ->
    "Fizz";
convert(N) when N rem 5 =:= 0 ->
    "Buzz";
convert(N) ->
    integer_to_list(N).

range(Start, End) ->
    lists:seq(Start, End).

fizzbuzz_list(Start, End) ->
    Numbers = range(Start, End),
    lists:map(fun convert/1, Numbers).