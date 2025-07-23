-module(fizzbuzz_type).

-export([convert_type1/1, convert_type2/1, convert_type3/1, check_fizz_buzz/1]).

convert_type1(N) ->
    {IsFizz, IsBuzz} = check_fizz_buzz(N),
    case {IsFizz, IsBuzz} of
        {true, true} -> "FizzBuzz";
        {true, false} -> "Fizz";
        {false, true} -> "Buzz";
        {false, false} -> integer_to_list(N)
    end.

convert_type2(N) ->
    integer_to_list(N).

convert_type3(N) ->
    {IsFizz, IsBuzz} = check_fizz_buzz(N),
    case {IsFizz, IsBuzz} of
        {true, true} -> "FizzBuzz";
        _ -> integer_to_list(N)
    end.

%% 共通処理の抽出
check_fizz_buzz(N) ->
    IsFizz = N rem 3 =:= 0,
    IsBuzz = N rem 5 =:= 0,
    {IsFizz, IsBuzz}.