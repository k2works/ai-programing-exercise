namespace AccountingSystem.Infrastructure.Adapters

open System
open System.Threading.Tasks
open AccountingSystem.Application.Repositories
open AccountingSystem.Domain.Models.Journal
open AccountingSystem.Infrastructure.Repositories

/// <summary>
/// 仕訳リポジトリアダプター（Output Adapter）
/// IJournalRepository インターフェースを実装し、
/// 既存の JournalRepository モジュール関数を呼び出す
/// </summary>
type JournalRepositoryAdapter(connectionString: string) =

    interface IJournalRepository with

        member _.SaveAsync(journal: Journal) : Task<string> =
            JournalRepository.insertAsync connectionString journal

        member _.FindByVoucherNumberAsync(voucherNumber: string) : Task<Journal option> =
            JournalRepository.findByVoucherNumberAsync connectionString voucherNumber

        member _.FindByDateRangeAsync(fromDate: DateTime, toDate: DateTime) : Task<Journal list> =
            JournalRepository.findByDateRangeAsync connectionString fromDate toDate

        member _.UpdateAsync(journal: Journal) : Task<int> =
            JournalRepository.updateAsync connectionString journal

        member _.DeleteAsync(voucherNumber: string) : Task<int> =
            JournalRepository.deleteAsync connectionString voucherNumber
