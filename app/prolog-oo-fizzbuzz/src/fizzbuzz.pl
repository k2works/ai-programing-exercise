% FizzBuzz 実装
% テスト駆動開発から始めるProlog入門3

% 1からNまでのFizzBuzzを出力
fizzbuzz_print(Start, End, Type) :-
    numlist(Start, End, Numbers),
    maplist(fizzbuzz_output(Type), Numbers).

% 単一の数値を出力
fizzbuzz_output(Type, Number) :-
    ( Type = 1 ->
        % タイプ1: 通常のFizzBuzz
        ( Number mod 15 =:= 0 ->
            Result = 'FizzBuzz'
        ; Number mod 3 =:= 0 ->
            Result = 'Fizz'
        ; Number mod 5 =:= 0 ->
            Result = 'Buzz'
        ;   atom_number(Result, Number)
        ),
        writeln(Result)
    ; Type = 2 ->
        % タイプ2: 数字のみ
        ( Number mod 3 =\= 0, Number mod 5 =\= 0 ->
            atom_number(Result, Number),
            writeln(Result)
        ;   true  % 何も出力しない
        )
    ).

% テスト用ヘルパー関数
fizzbuzz_get_value(Number, Type, Result) :-
    ( Type = 1 ->
        ( Number mod 15 =:= 0 ->
            Result = 'FizzBuzz'
        ; Number mod 3 =:= 0 ->
            Result = 'Fizz'
        ; Number mod 5 =:= 0 ->
            Result = 'Buzz'
        ;   atom_number(Result, Number)
        )
    ; Type = 2 ->
        % タイプ2: 数字のみ（3または5の倍数以外）
        Number mod 3 =\= 0,
        Number mod 5 =\= 0,
        atom_number(Result, Number)
    ).
