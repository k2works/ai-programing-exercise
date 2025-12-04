namespace AccountingSystem.Infrastructure.Adapters

open System.Threading.Tasks
open AccountingSystem.Application.Port.Out
open AccountingSystem.Domain.Models
open AccountingSystem.Infrastructure.Persistence.Repositories

/// <summary>
/// 勘定科目リポジトリアダプター（Output Adapter）
/// IAccountRepository インターフェースを実装し、
/// 既存の AccountRepository モジュール関数を呼び出す
/// </summary>
type AccountRepositoryAdapter(connectionString: string) =

    interface IAccountRepository with

        member _.FindAllAsync() : Task<Account list> =
            AccountRepository.findAllAsync connectionString

        member _.FindByCodeAsync(accountCode: string) : Task<Account option> =
            AccountRepository.findByCodeAsync connectionString accountCode

        member _.FindByIdAsync(accountId: int) : Task<Account option> =
            AccountRepository.findByIdAsync connectionString accountId

        member _.FindByTypeAsync(accountType: string) : Task<Account list> =
            AccountRepository.findByTypeAsync connectionString accountType

        member _.SaveAsync(account: Account) : Task<int> =
            AccountRepository.insertAsync connectionString account

        member _.UpdateAsync(account: Account) : Task<int> =
            AccountRepository.updateAsync connectionString account

        member _.DeleteByCodeAsync(accountCode: string) : Task<int> =
            AccountRepository.deleteAsync connectionString accountCode
