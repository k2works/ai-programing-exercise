-module(fizzbuzz).
-export([convert/1, generate/2, print/0]).

convert(N) when N rem 15 =:= 0 ->
    "FizzBuzz";
convert(N) when N rem 3 =:= 0 ->
    "Fizz";
convert(N) when N rem 5 =:= 0 ->
    "Buzz";
convert(N) ->
    integer_to_list(N).

generate(Start, End) ->
    [convert(N) || N <- lists:seq(Start, End)].

print() ->
    FizzBuzzList = generate(1, 100),
    lists:foreach(fun(Item) -> io:format("~s~n", [Item]) end, FizzBuzzList).