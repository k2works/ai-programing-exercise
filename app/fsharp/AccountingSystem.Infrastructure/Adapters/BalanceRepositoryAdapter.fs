namespace AccountingSystem.Infrastructure.Adapters

open AccountingSystem.Application.Port.Out
open AccountingSystem.Infrastructure.Persistence.Repositories

/// <summary>
/// IBalanceRepository インターフェースのアダプター実装
/// module 関数をインターフェース経由で利用可能にする
/// </summary>
type BalanceRepositoryAdapter(connectionString: string) =
    interface IBalanceRepository with
        member _.UpdateDailyBalanceAsync(param) =
            BalanceRepository.updateDailyBalanceAsync connectionString param

        member _.UpdateBalanceFromJournalItemsAsync(journalNo) =
            BalanceRepository.updateBalanceFromJournalItemsAsync connectionString journalNo

        member _.ConsolidateMonthlyBalanceAsync(param) =
            BalanceRepository.consolidateMonthlyBalanceAsync connectionString param
