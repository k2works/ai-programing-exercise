% FizzBuzz テスト
% テスト駆動開発から始めるProlog入門3

:- use_module(library(plunit)).

% テストファイルから見えるようにソースファイルを読み込み
:- consult('../src/fizzbuzz.pl').

:- begin_tests(fizzbuzz_type1).

test('タイプ1: 1から100までのFizzBuzzを出力') :-
    fizzbuzz_print(1, 100, 1).

test('タイプ1: 1を渡したら1を出力') :-
    fizzbuzz_get_value(1, 1, Result),
    assertion(Result = '1').

test('タイプ1: 3を渡したらFizzを出力') :-
    fizzbuzz_get_value(3, 1, Result),
    assertion(Result = 'Fizz').

test('タイプ1: 5を渡したらBuzzを出力') :-
    fizzbuzz_get_value(5, 1, Result),
    assertion(Result = 'Buzz').

test('タイプ1: 15を渡したらFizzBuzzを出力') :-
    fizzbuzz_get_value(15, 1, Result),
    assertion(Result = 'FizzBuzz').

:- end_tests(fizzbuzz_type1).

:- begin_tests(fizzbuzz_type2).

test('タイプ2: 1から100までの数字のみを出力') :-
    fizzbuzz_print(1, 100, 2).

test('タイプ2: 1を渡したら1を出力') :-
    fizzbuzz_get_value(1, 2, Result),
    assertion(Result = '1').

test('タイプ2: 3を渡したら何も出力しない', [fail]) :-
    fizzbuzz_get_value(3, 2, _).

test('タイプ2: 5を渡したら何も出力しない', [fail]) :-
    fizzbuzz_get_value(5, 2, _).

test('タイプ2: 15を渡したら何も出力しない', [fail]) :-
    fizzbuzz_get_value(15, 2, _).

:- end_tests(fizzbuzz_type2).

:- begin_tests(fizzbuzz_type3).

test('タイプ3: 1から100までのFizzBuzzのみを出力') :-
    fizzbuzz_print(1, 100, 3).

test('タイプ3: 1を渡したら何も出力しない', [fail]) :-
    fizzbuzz_get_value(1, 3, _).

test('タイプ3: 3を渡したら何も出力しない', [fail]) :-
    fizzbuzz_get_value(3, 3, _).

test('タイプ3: 5を渡したら何も出力しない', [fail]) :-
    fizzbuzz_get_value(5, 3, _).

test('タイプ3: 15を渡したらFizzBuzzを出力') :-
    fizzbuzz_get_value(15, 3, Result),
    assertion(Result = 'FizzBuzz').

:- end_tests(fizzbuzz_type3).

:- begin_tests(fizzbuzz_type4).

test('タイプ4: 1から100までの数字とFizzBuzzのみを出力') :-
    fizzbuzz_print(1, 100, 4).

test('タイプ4: 1を渡したら1を出力') :-
    fizzbuzz_get_value(1, 4, Result),
    assertion(Result = '1').

test('タイプ4: 3を渡したら何も出力しない', [fail]) :-
    fizzbuzz_get_value(3, 4, _).

test('タイプ4: 5を渡したら何も出力しない', [fail]) :-
    fizzbuzz_get_value(5, 4, _).

test('タイプ4: 15を渡したらFizzBuzzを出力') :-
    fizzbuzz_get_value(15, 4, Result),
    assertion(Result = 'FizzBuzz').

:- end_tests(fizzbuzz_type4).
