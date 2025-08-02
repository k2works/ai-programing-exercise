% FizzBuzz テスト
% テスト駆動開発から始めるProlog入門3

:- use_module(library(plunit)).

% テストファイルから見えるようにソースファイルを読み込み
:- consult('../src/fizzbuzz.pl').

:- begin_tests(fizzbuzz_type1).

test('タイプ1: 1から100までのFizzBuzzを出力') :-
    fizzbuzz_print(1, 100, 1).

test('タイプ1: 1を渡したら1を出力') :-
    fizzbuzz_convert(1, 1, Result),
    assertion(Result = '1').

test('タイプ1: 3を渡したらFizzを出力') :-
    fizzbuzz_convert(3, 1, Result),
    assertion(Result = 'Fizz').

test('タイプ1: 5を渡したらBuzzを出力') :-
    fizzbuzz_convert(5, 1, Result),
    assertion(Result = 'Buzz').

test('タイプ1: 15を渡したらFizzBuzzを出力') :-
    fizzbuzz_convert(15, 1, Result),
    assertion(Result = 'FizzBuzz').

:- end_tests(fizzbuzz_type1).
