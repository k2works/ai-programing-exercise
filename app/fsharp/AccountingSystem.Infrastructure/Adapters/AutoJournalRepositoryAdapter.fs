namespace AccountingSystem.Infrastructure.Adapters

open AccountingSystem.Application.Port.Out
open AccountingSystem.Infrastructure.Persistence.Repositories

/// <summary>
/// IAutoJournalRepository インターフェースのアダプター実装
/// module 関数をインターフェース経由で利用可能にする
/// </summary>
type AutoJournalRepositoryAdapter(connectionString: string) =
    interface IAutoJournalRepository with
        member _.GetPatternsAsync(query) =
            AutoJournalRepository.getPatternsAsync connectionString query

        member _.GetPatternByCodeAsync(patternCode) =
            AutoJournalRepository.getPatternByCodeAsync connectionString patternCode

        member _.GetPatternItemsAsync(patternId) =
            AutoJournalRepository.getPatternItemsAsync connectionString patternId

        member _.SavePatternAsync(pattern) =
            AutoJournalRepository.savePatternAsync connectionString pattern

        member _.SavePatternItemsAsync patternId items =
            AutoJournalRepository.savePatternItemsAsync connectionString patternId items

        member _.SaveLogAsync(log) =
            AutoJournalRepository.saveLogAsync connectionString log

        member _.GetLogsByPatternIdAsync patternId limit =
            AutoJournalRepository.getLogsByPatternIdAsync connectionString patternId limit
