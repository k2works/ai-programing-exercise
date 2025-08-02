% パフォーマンステスト
% テスト駆動開発から始めるProlog入門2

:- use_module(library(plunit)).
:- consult(fizzbuzz).
:- consult(utils).

% パフォーマンステスト
:- begin_tests(performance, [blocked('パフォーマンステストは手動実行のみ')]).

test('大量データの処理性能') :-
    get_time(Start),
    fizzbuzz_list(1, 10000, _),
    get_time(End),
    Duration is End - Start,
    format('10,000個の処理時間: ~3f秒~n', [Duration]),
    Duration < 1.0.  % 1秒以内で完了することを期待

test('メモリ効率性') :-
    % 大きなリストでもメモリ不足にならないことを確認
    fizzbuzz_list(1, 50000, Result),
    length(Result, 50000).

:- end_tests(performance).
