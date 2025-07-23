-module(fizzbuzz_type).

-export([convert_type1/1, convert_type2/1, convert_type3/1, check_fizz_buzz/1,
         pipeline_convert/2, compose/2, apply_rules/3]).

%% 関数合成とパイプライン処理
compose(F, G) ->
    fun(X) -> F(G(X)) end.

pipeline_convert(N, Type) ->
    ComposedFun = compose(
        fun(Conditions) -> apply_rules(Conditions, N, Type) end,
        fun check_fizz_buzz/1
    ),
    ComposedFun(N).

apply_rules({IsFizz, IsBuzz}, N, 1) ->
    case {IsFizz, IsBuzz} of
        {true, true} -> "FizzBuzz";
        {true, false} -> "Fizz";
        {false, true} -> "Buzz";
        {false, false} -> integer_to_list(N)
    end;
apply_rules(_, N, 2) ->
    integer_to_list(N);
apply_rules({IsFizz, IsBuzz}, N, 3) ->
    case {IsFizz, IsBuzz} of
        {true, true} -> "FizzBuzz";
        _ -> integer_to_list(N)
    end.

convert_type1(N) ->
    pipeline_convert(N, 1).

convert_type2(N) ->
    pipeline_convert(N, 2).

convert_type3(N) ->
    pipeline_convert(N, 3).

%% 共通処理の抽出
check_fizz_buzz(N) ->
    IsFizz = N rem 3 =:= 0,
    IsBuzz = N rem 5 =:= 0,
    {IsFizz, IsBuzz}.