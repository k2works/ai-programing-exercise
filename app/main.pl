% FizzBuzz テスト駆動開発 - Prolog版
% テスティングフレームワークplunitを使用

:- use_module(library(plunit)).

% FizzBuzzテスト
:- begin_tests(fizzbuzz).

test('1を渡したら文字列1を返す') :-
    fizzbuzz_generate(1, Result),
    Result = '1'.

:- end_tests(fizzbuzz).

% FizzBuzz実装（仮実装）
fizzbuzz_generate(1, '1').

% テスト実行用
run_all_tests :-
    run_tests.
