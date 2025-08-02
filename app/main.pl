% FizzBuzz テスト駆動開発 - Prolog版
% テスティングフレームワークplunitを使用

:- use_module(library(plunit)).

% 最初のテスト: セットアップ
:- begin_tests(hello).

test(greeting) :-
    greeting(Result),
    Result = 'hello world'.

:- end_tests(hello).

% セットアップ用のプレディケート
greeting('hello world').

% テストを実行するためのプレディケート
run_tests :-
    run_tests(hello).
