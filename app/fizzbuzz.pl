% FizzBuzz プロダクトコード
% テスト駆動開発から始めるProlog入門2

% 定数定義
max_number(100).

% FizzBuzz実装
fizzbuzz_generate(Number, Result) :-
    ( is_fizzbuzz(Number) ->
        Result = 'FizzBuzz'
    ; is_fizz(Number) ->
        Result = 'Fizz'
    ; is_buzz(Number) ->
        Result = 'Buzz'
    ;   atom_number(Result, Number)
    ).

% ヘルパー述語
is_fizzbuzz(Number) :-
    0 =:= Number mod 3,
    0 =:= Number mod 5.

is_fizz(Number) :-
    0 =:= Number mod 3.

is_buzz(Number) :-
    0 =:= Number mod 5.

% 1からMaxまでのFizzBuzz配列を一発で作る
fizzbuzz_list(Start, End, Result) :-
    numlist(Start, End, Numbers),
    maplist(fizzbuzz_generate, Numbers, Result).

% FizzBuzzをプリント
fizzbuzz_print(Start, End) :-
    fizzbuzz_list(Start, End, Result),
    maplist(writeln, Result).

% タイプ別プリント機能
fizzbuzz_print_type(Start, End, Type) :-
    fizzbuzz_list(Start, End, Result),
    filter_by_type(Result, Type, FilteredResult),
    maplist(writeln, FilteredResult).

% タイプ別フィルタリング
filter_by_type(List, 1, List).  % タイプ1: 全て
filter_by_type(List, 2, NumbersOnly) :-  % タイプ2: 数字のみ
    include(is_number_string, List, NumbersOnly).
filter_by_type(List, 3, FizzBuzzOnly) :-  % タイプ3: FizzBuzzのみ
    include(=('FizzBuzz'), List, FizzBuzzOnly).

% 数字文字列かどうかを判定
is_number_string(String) :-
    atom_number(String, _).
