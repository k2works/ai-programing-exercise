% FizzBuzz テストスイート
% テスト駆動開発から始めるProlog入門2

:- use_module(library(plunit)).
:- consult(fizzbuzz).
:- consult(utils).

% 基本的なFizzBuzzテスト
:- begin_tests(fizzbuzz_basic).

test('1を渡したら文字列1を返す') :-
    fizzbuzz_generate(1, Result),
    assertion(Result = '1').

test('2を渡したら文字列2を返す') :-
    fizzbuzz_generate(2, Result),
    assertion(Result = '2').

test('3を渡したら文字列Fizzを返す') :-
    fizzbuzz_generate(3, Result),
    assertion(Result = 'Fizz').

test('5を渡したら文字列Buzzを返す') :-
    fizzbuzz_generate(5, Result),
    assertion(Result = 'Buzz').

test('15を渡したら文字列FizzBuzzを返す') :-
    fizzbuzz_generate(15, Result),
    assertion(Result = 'FizzBuzz').

test('6を渡したら文字列Fizzを返す') :-
    fizzbuzz_generate(6, Result),
    assertion(Result = 'Fizz').

test('10を渡したら文字列Buzzを返す') :-
    fizzbuzz_generate(10, Result),
    assertion(Result = 'Buzz').

test('30を渡したら文字列FizzBuzzを返す') :-
    fizzbuzz_generate(30, Result),
    assertion(Result = 'FizzBuzz').

:- end_tests(fizzbuzz_basic).

% リスト処理テスト
:- begin_tests(fizzbuzz_list).

test('リストの初めは文字列の1を返す') :-
    fizzbuzz_list(1, 100, Result),
    Result = [H|_],
    assertion(H = '1').

test('リストの最後は文字列のBuzzを返す') :-
    fizzbuzz_list(1, 100, Result),
    last(Result, Last),
    assertion(Last = 'Buzz').

test('リストの3番目は文字列のFizzを返す') :-
    fizzbuzz_list(1, 100, Result),
    nth1(3, Result, Third),
    assertion(Third = 'Fizz').

test('リストの5番目は文字列のBuzzを返す') :-
    fizzbuzz_list(1, 100, Result),
    nth1(5, Result, Fifth),
    assertion(Fifth = 'Buzz').

test('リストの15番目は文字列のFizzBuzzを返す') :-
    fizzbuzz_list(1, 100, Result),
    nth1(15, Result, Fifteenth),
    assertion(Fifteenth = 'FizzBuzz').

test('1から10までのリストの長さは10') :-
    fizzbuzz_list(1, 10, Result),
    length(Result, Length),
    assertion(Length = 10).

test('1から1までのリストは1つの要素') :-
    fizzbuzz_list(1, 1, Result),
    assertion(Result = ['1']).

:- end_tests(fizzbuzz_list).

% タイプ別フィルタリングテスト
:- begin_tests(fizzbuzz_type_filter).

test('タイプ1は全ての要素を返す') :-
    fizzbuzz_list(1, 5, List),
    filter_by_type(List, 1, Result),
    assertion(Result = ['1', '2', 'Fizz', '4', 'Buzz']).

test('タイプ2は数字のみを返す') :-
    fizzbuzz_list(1, 5, List),
    filter_by_type(List, 2, Result),
    assertion(Result = ['1', '2', '4']).

test('タイプ3はFizzBuzzのみを返す') :-
    fizzbuzz_list(1, 15, List),
    filter_by_type(List, 3, Result),
    assertion(Result = ['FizzBuzz']).

test('1から30までのタイプ3はFizzBuzzが2つ') :-
    fizzbuzz_list(1, 30, List),
    filter_by_type(List, 3, Result),
    length(Result, Length),
    assertion(Length = 2).

:- end_tests(fizzbuzz_type_filter).

% エッジケーステスト
:- begin_tests(fizzbuzz_edge_cases).

test('0を渡したら文字列FizzBuzzを返す') :-
    fizzbuzz_generate(0, Result),
    assertion(Result = 'FizzBuzz').

test('負の数-3を渡したら文字列Fizzを返す') :-
    fizzbuzz_generate(-3, Result),
    assertion(Result = 'Fizz').

test('負の数-5を渡したら文字列Buzzを返す') :-
    fizzbuzz_generate(-5, Result),
    assertion(Result = 'Buzz').

test('負の数-15を渡したら文字列FizzBuzzを返す') :-
    fizzbuzz_generate(-15, Result),
    assertion(Result = 'FizzBuzz').

:- end_tests(fizzbuzz_edge_cases).

% バリデーションテスト
:- begin_tests(validation).

test('正常な範囲の検証は成功する') :-
    validate_range(1, 10).

test('開始値が終了値より大きい場合は失敗する', [fail]) :-
    validate_range(10, 5).

test('正常なタイプの検証は成功する') :-
    validate_type(1),
    validate_type(2),
    validate_type(3).

test('無効なタイプの検証は失敗する', [fail]) :-
    validate_type(4).

test('文字列のタイプは失敗する', [fail]) :-
    validate_type('invalid').

:- end_tests(validation).

% ユーティリティテスト
:- begin_tests(utilities).

test('数字文字列の判定が正しく動作する') :-
    is_number_string('1'),
    is_number_string('42'),
    \+ is_number_string('Fizz'),
    \+ is_number_string('Buzz').

test('統計情報の計算が正しい') :-
    fizzbuzz_list(1, 15, List),
    include(=('Fizz'), List, FizzList),
    include(=('Buzz'), List, BuzzList),
    include(=('FizzBuzz'), List, FizzBuzzList),
    include(is_number_string, List, NumberList),
    length(FizzList, 4),    % 3, 6, 9, 12
    length(BuzzList, 2),    % 5, 10
    length(FizzBuzzList, 1), % 15
    length(NumberList, 8).  % 1, 2, 4, 7, 8, 11, 13, 14

:- end_tests(utilities).
