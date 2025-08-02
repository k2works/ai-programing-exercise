% FizzBuzz メインアプリケーション
% テスト駆動開発から始めるProlog入門2

:- use_module(fizzbuzz).

% メイン実行関数
main :-
    % デフォルトパラメータで実行
    fizzbuzz_print(1, 100).

% パラメータ付きメイン関数
main(Args) :-
    parse_args(Args, Start, End, Type),
    execute_fizzbuzz(Start, End, Type).

% 引数の解析
parse_args([], 1, 100, 1).  % デフォルト値
parse_args([StartAtom], Start, 100, 1) :-  % 開始値のみ指定
    atom_number(StartAtom, Start).
parse_args([StartAtom, EndAtom], Start, End, 1) :-  % 開始値と終了値指定
    atom_number(StartAtom, Start),
    atom_number(EndAtom, End).
parse_args([StartAtom, EndAtom, TypeAtom], Start, End, Type) :-  % 全パラメータ指定
    atom_number(StartAtom, Start),
    atom_number(EndAtom, End),
    atom_number(TypeAtom, Type).

% FizzBuzzの実行
execute_fizzbuzz(Start, End, Type) :-
    ( Type = 1 ->
        fizzbuzz_print(Start, End)
    ;   fizzbuzz_print_type(Start, End, Type)
    ).

% コマンドライン実行のためのヘルパー
run_fizzbuzz :-
    current_prolog_flag(argv, Argv),
    main(Argv).

% 対話的実行のためのヘルパー
run_fizzbuzz(Start, End) :-
    fizzbuzz_print(Start, End).

run_fizzbuzz(Start, End, Type) :-
    fizzbuzz_print_type(Start, End, Type).

% テスト実行
run_tests :-
    run_tests([test_fizzbuzz]).

% 使用方法の表示
usage :-
    writeln('使用方法: swipl -g "main(Args)" -t halt app.pl'),
    writeln(''),
    writeln('引数:'),
    writeln('  引数なし        : 1から100までのFizzBuzzを出力（タイプ1）'),
    writeln('  [開始値]        : 指定した開始値から100までのFizzBuzzを出力'),
    writeln('  [開始値] [終了値] : 指定した範囲のFizzBuzzを出力'),
    writeln('  [開始値] [終了値] [タイプ] : 指定した範囲とタイプでFizzBuzzを出力'),
    writeln(''),
    writeln('タイプ:'),
    writeln('  1 : 通常のFizzBuzz（デフォルト）'),
    writeln('  2 : 数字のみ'),
    writeln('  3 : FizzBuzzのみ'),
    writeln(''),
    writeln('例:'),
    writeln('  swipl -g "main" -t halt app.pl'),
    writeln('  swipl -g "main([''1'', ''20''])" -t halt app.pl'),
    writeln('  swipl -g "main([''1'', ''30'', ''2''])" -t halt app.pl').
