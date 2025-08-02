% FizzBuzz テスト駆動開発 - Prolog版
% テスティングフレームワークplunitを使用

:- use_module(library(plunit)).

% FizzBuzzテスト
:- begin_tests(fizzbuzz).

test('1を渡したら文字列1を返す') :-
    fizzbuzz_generate(1, Result),
    Result = '1'.

test('2を渡したら文字列2を返す') :-
    fizzbuzz_generate(2, Result),
    Result = '2'.

test('3を渡したら文字列Fizzを返す') :-
    fizzbuzz_generate(3, Result),
    Result = 'Fizz'.

:- end_tests(fizzbuzz).

% FizzBuzz実装
fizzbuzz_generate(Number, Result) :-
    ( Number mod 3 =:= 0 ->
        Result = 'Fizz'
    ;   atom_number(Result, Number)
    ).

% テスト実行用
run_all_tests :-
    run_tests.
