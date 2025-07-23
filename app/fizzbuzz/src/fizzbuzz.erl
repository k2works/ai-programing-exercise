-module(fizzbuzz).

-export([convert/1, convert/2, fizzbuzz_list/0, print_fizzbuzz/0]).

convert(N) ->
    convert(N, 1).

convert(N, Type) ->
    case Type of
        1 -> convert_type1(N);
        2 -> convert_type2(N);
        3 -> convert_type3(N)
    end.

convert_type1(N) ->
    IsFizz = N rem 3 =:= 0,
    IsBuzz = N rem 5 =:= 0,
    case {IsFizz, IsBuzz} of
        {true, true} -> "FizzBuzz";
        {true, false} -> "Fizz";
        {false, true} -> "Buzz";
        {false, false} -> integer_to_list(N)
    end.

convert_type2(N) ->
    integer_to_list(N).

convert_type3(N) ->
    IsFizz = N rem 3 =:= 0,
    IsBuzz = N rem 5 =:= 0,
    case {IsFizz, IsBuzz} of
        {true, true} -> "FizzBuzz";
        _ -> integer_to_list(N)
    end.

fizzbuzz_list() ->
    lists:map(fun convert/1, lists:seq(1, 100)).

print_fizzbuzz() ->
    List = fizzbuzz_list(),
    lists:foreach(fun(Item) -> io:format("~s~n", [Item]) end, List),
    ok.