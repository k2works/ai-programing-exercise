% FizzBuzz 実装
% テスト駆動開発から始めるProlog入門3

% FizzBuzz変換（タイプ1の場合）
fizzbuzz_convert(Number, Type, Result) :-
    Type = 1,
    ( Number mod 15 =:= 0 ->
        Result = 'FizzBuzz'
    ; Number mod 3 =:= 0 ->
        Result = 'Fizz'
    ; Number mod 5 =:= 0 ->
        Result = 'Buzz'
    ;   atom_number(Result, Number)
    ).

% 1からNまでのFizzBuzzを出力
fizzbuzz_print(Start, End, Type) :-
    numlist(Start, End, Numbers),
    maplist(fizzbuzz_output(Type), Numbers).

% 単一の数値を出力
fizzbuzz_output(Type, Number) :-
    fizzbuzz_convert(Number, Type, Result),
    writeln(Result).
