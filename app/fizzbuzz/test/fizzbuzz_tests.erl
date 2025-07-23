-module(fizzbuzz_tests).
-include_lib("eunit/include/eunit.hrl").

learning_test() ->
    ?assertEqual([1,2,3], lists:seq(1, 3)),
    ?assertEqual(["1","2","Fizz"], lists:map(fun fizzbuzz:convert/1, lists:seq(1, 3))).

convert_test() ->
    ?assertEqual("1", fizzbuzz:convert(1)),
    ?assertEqual("2", fizzbuzz:convert(2)),
    ?assertEqual("Fizz", fizzbuzz:convert(3)),
    ?assertEqual("Buzz", fizzbuzz:convert(5)),
    ?assertEqual("FizzBuzz", fizzbuzz:convert(15)).

fizzbuzz_list_test() ->
    List = fizzbuzz:fizzbuzz_list(),
    ?assertEqual("1", lists:nth(1, List)),
    ?assertEqual("Buzz", lists:nth(100, List)).

print_fizzbuzz_test() ->
    ?assertEqual(ok, fizzbuzz:print_fizzbuzz()).

%% タイプごとに出力を切り替えることができる
%% タイプ1の場合
type1_test() ->
    ?assertEqual("1", fizzbuzz:convert(1, 1)).

%% タイプ2の場合 - 数字のみを返す
type2_test() ->
    ?assertEqual("1", fizzbuzz:convert(1, 2)),
    ?assertEqual("3", fizzbuzz:convert(3, 2)),
    ?assertEqual("5", fizzbuzz:convert(5, 2)),
    ?assertEqual("15", fizzbuzz:convert(15, 2)).

%% タイプ3の場合 - FizzBuzzの場合のみFizzBuzzを返し、それ以外は数字を返す
type3_test() ->
    ?assertEqual("1", fizzbuzz:convert(1, 3)),
    ?assertEqual("3", fizzbuzz:convert(3, 3)),
    ?assertEqual("5", fizzbuzz:convert(5, 3)),
    ?assertEqual("FizzBuzz", fizzbuzz:convert(15, 3)).

%% それ以外のタイプの場合 - 例外を投げる
type_other_test() ->
    ?assertError({case_clause, 4}, fizzbuzz:convert(1, 4)).