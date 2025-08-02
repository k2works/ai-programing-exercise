% エラーハンドリングとバリデーション機能
% テスト駆動開発から始めるProlog入門2

% 入力値のバリデーション
validate_range(Start, End) :-
    ( integer(Start), integer(End) ->
        ( Start =< End ->
            true
        ;   format('エラー: 開始値（~w）は終了値（~w）以下である必要があります~n', [Start, End]),
            fail
        )
    ;   format('エラー: 開始値と終了値は整数である必要があります~n'),
        fail
    ).

validate_type(Type) :-
    ( integer(Type) ->
        ( member(Type, [1, 2, 3]) ->
            true
        ;   format('エラー: タイプは1、2、または3である必要があります~n'),
            fail
        )
    ;   format('エラー: タイプは整数である必要があります~n'),
        fail
    ).

% 安全な実行用ラッパー
safe_fizzbuzz_print(Start, End, Type) :-
    catch(
        ( validate_range(Start, End),
          validate_type(Type),
          execute_fizzbuzz(Start, End, Type)
        ),
        Error,
        ( format('予期しないエラーが発生しました: ~w~n', [Error]),
          fail
        )
    ).

% パフォーマンス測定機能
benchmark_fizzbuzz(Start, End) :-
    get_time(StartTime),
    fizzbuzz_list(Start, End, _),
    get_time(EndTime),
    Duration is EndTime - StartTime,
    Count is End - Start + 1,
    format('~w個の数値を処理するのに~3f秒かかりました~n', [Count, Duration]).

% 統計情報の表示
show_stats(Start, End) :-
    fizzbuzz_list(Start, End, Result),
    length(Result, Total),
    include(=('Fizz'), Result, FizzList),
    include(=('Buzz'), Result, BuzzList),
    include(=('FizzBuzz'), Result, FizzBuzzList),
    include(is_number_string, Result, NumberList),
    length(FizzList, FizzCount),
    length(BuzzList, BuzzCount),
    length(FizzBuzzList, FizzBuzzCount),
    length(NumberList, NumberCount),
    format('統計情報（~w-~w）:~n', [Start, End]),
    format('  総数: ~w~n', [Total]),
    format('  数字: ~w~n', [NumberCount]),
    format('  Fizz: ~w~n', [FizzCount]),
    format('  Buzz: ~w~n', [BuzzCount]),
    format('  FizzBuzz: ~w~n', [FizzBuzzCount]).
