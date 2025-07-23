-module(fizzbuzz_tests).
-include_lib("eunit/include/eunit.hrl").

learning_test() ->
    ?assertEqual([1,2,3], lists:seq(1, 3)),
    ?assertEqual(["1","2","Fizz"], lists:map(fun fizzbuzz:generate/1, lists:seq(1, 3))).

generate_test() ->
    ?assertEqual("1", fizzbuzz:generate(1)),
    ?assertEqual("2", fizzbuzz:generate(2)),
    ?assertEqual("Fizz", fizzbuzz:generate(3)),
    ?assertEqual("Buzz", fizzbuzz:generate(5)),
    ?assertEqual("FizzBuzz", fizzbuzz:generate(15)).

create_list_test() ->
    List = fizzbuzz:create_list(),
    ?assertEqual("1", lists:nth(1, List)),
    ?assertEqual("Buzz", lists:nth(100, List)).

print_fizzbuzz_test() ->
    ?assertEqual(ok, fizzbuzz:print_fizzbuzz()).

%% タイプごとに出力を切り替えることができる
%% タイプ1の場合
type1_test() ->
    ?assertEqual("1", fizzbuzz:generate(1, 1)).

%% タイプ2の場合 - 数字のみを返す
type2_test() ->
    ?assertEqual("1", fizzbuzz:generate(1, 2)),
    ?assertEqual("3", fizzbuzz:generate(3, 2)),
    ?assertEqual("5", fizzbuzz:generate(5, 2)),
    ?assertEqual("15", fizzbuzz:generate(15, 2)).

%% タイプ3の場合 - FizzBuzzの場合のみFizzBuzzを返し、それ以外は数字を返す
type3_test() ->
    ?assertEqual("1", fizzbuzz:generate(1, 3)),
    ?assertEqual("3", fizzbuzz:generate(3, 3)),
    ?assertEqual("5", fizzbuzz:generate(5, 3)),
    ?assertEqual("FizzBuzz", fizzbuzz:generate(15, 3)).

%% それ以外のタイプの場合 - 例外を投げる
type_other_test() ->
    ?assertError(function_clause, fizzbuzz:generate(1, 4)).

%% Value Object tests
value_object_test() ->
    Number = fizzbuzz:create_fizzbuzz_number(3),
    Type = fizzbuzz:create_fizzbuzz_type(1),
    ?assertEqual("Fizz", fizzbuzz:generate_with_value_objects(Number, Type)).

value_object_validation_test() ->
    ?assertError(function_clause, fizzbuzz:create_fizzbuzz_number(0)),
    ?assertError(function_clause, fizzbuzz:create_fizzbuzz_type(4)).

%% 関数型リファクタリングのテスト
functional_features_test() ->
    % パイプライン処理のテスト
    ?assertEqual("Fizz", fizzbuzz_type:pipeline_convert(3, 1)),
    
    % 高階関数のテスト
    List = fizzbuzz:create_list(1, 5),
    ?assertEqual(5, length(List)),
    
    % リスト内包表記のテスト
    CompList = fizzbuzz:create_list_comprehension(),
    ?assertEqual(100, length(CompList)),
    
    % フィルタリングのテスト
    FizzBuzzList = fizzbuzz:filter_only_fizzbuzz(),
    ?assert(length(FizzBuzzList) > 0),
    
    % 遅延評価のテスト
    LazyGen = fizzbuzz:create_lazy_list(1, 5),
    ?assert(is_function(LazyGen)),
    
    % エラーハンドリングのテスト
    {ok, _Number} = fizzbuzz_value:safe_create_fizzbuzz_number(5),
    ?assertEqual({error, invalid_number}, fizzbuzz_value:safe_create_fizzbuzz_number(-1)),
    
    % 関数合成のテスト
    Compose = fizzbuzz_type:compose(fun(X) -> X + 1 end, fun(X) -> X * 2 end),
    ?assertEqual(11, Compose(5)).

maybe_monad_test() ->
    % Maybe Monadのテスト
    Result1 = fizzbuzz_value:maybe_bind(
        fizzbuzz_value:safe_create_fizzbuzz_number(3),
        fun(_) -> {ok, "success"} end
    ),
    ?assertEqual({ok, "success"}, Result1),
    
    Result2 = fizzbuzz_value:maybe_bind(
        fizzbuzz_value:safe_create_fizzbuzz_number(-1),
        fun(_) -> {ok, "success"} end
    ),
    ?assertEqual({error, invalid_number}, Result2).