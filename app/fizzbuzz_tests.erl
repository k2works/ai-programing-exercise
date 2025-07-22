-module(fizzbuzz_tests).
-include_lib("eunit/include/eunit.hrl").

convert_test() ->
    ?assertEqual("1", fizzbuzz:convert(1)),
    ?assertEqual("2", fizzbuzz:convert(2)),
    ?assertEqual("Fizz", fizzbuzz:convert(3)),
    ?assertEqual("Buzz", fizzbuzz:convert(5)),
    ?assertEqual("FizzBuzz", fizzbuzz:convert(15)).

generate_test() ->
    Expected = ["1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", 
                "11", "Fizz", "13", "14", "FizzBuzz"],
    ?assertEqual(Expected, fizzbuzz:generate(1, 15)).

learning_test() ->
    % リスト操作の学習
    Numbers = [1, 2, 3, 4, 5],
    ?assertEqual([2, 4, 6, 8, 10], lists:map(fun(X) -> X * 2 end, Numbers)),
    
    % リスト内包表記の学習
    Doubles = [X * 2 || X <- Numbers],
    ?assertEqual([2, 4, 6, 8, 10], Doubles),
    
    % フィルタリングの学習
    Evens = [X || X <- Numbers, X rem 2 =:= 0],
    ?assertEqual([2, 4], Evens).

print_test() ->
    % プリント機能は副作用があるので、正常に実行されることを確認
    ?assertMatch(ok, fizzbuzz:print()).