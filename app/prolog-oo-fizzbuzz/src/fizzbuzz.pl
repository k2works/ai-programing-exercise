% FizzBuzz 実装
% テスト駆動開発から始めるProlog入門3

% 1からNまでのFizzBuzzを出力
fizzbuzz_print(Start, End, Type) :-
    numlist(Start, End, Numbers),
    maplist(fizzbuzz_output(Type), Numbers).

% 単一の数値を出力
fizzbuzz_output(Type, Number) :-
    fizzbuzz_calculate(Number, Type, Output),
    ( Output \= none ->
        writeln(Output)
    ;   true
    ).

% FizzBuzz計算ロジック
fizzbuzz_calculate(Number, Type, Output) :-
    ( Type = 1 ->
        % タイプ1: 通常のFizzBuzz
        ( Number mod 15 =:= 0 ->
            Output = 'FizzBuzz'
        ; Number mod 3 =:= 0 ->
            Output = 'Fizz'
        ; Number mod 5 =:= 0 ->
            Output = 'Buzz'
        ;   atom_number(Output, Number)
        )
    ; Type = 2 ->
        % タイプ2: 数字のみ
        ( Number mod 3 =\= 0, Number mod 5 =\= 0 ->
            atom_number(Output, Number)
        ;   Output = none
        )
    ; Type = 3 ->
        % タイプ3: FizzBuzzのみ
        ( Number mod 15 =:= 0 ->
            Output = 'FizzBuzz'
        ;   Output = none
        )
    ).

% テスト用ヘルパー関数
fizzbuzz_get_value(Number, Type, Result) :-
    fizzbuzz_calculate(Number, Type, Output),
    Output \= none,
    Result = Output.
