module AccountingSystem.Tests.Application.AccountServiceTest

open System
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Types
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services
open AccountingSystem.Application.Exceptions

/// <summary>
/// モックリポジトリ
/// </summary>
type MockAccountRepository(accounts: Account list ref) =
    let mutable nextId = 1

    interface IAccountRepository with
        member _.FindAllAsync() =
            Task.FromResult(!accounts)

        member _.FindByCodeAsync(accountCode: string) =
            let found = !accounts |> List.tryFind (fun a -> a.AccountCode.Code = accountCode)
            Task.FromResult(found)

        member _.FindByIdAsync(accountId: int) =
            let found = !accounts |> List.tryFind (fun a -> a.AccountId = Some accountId)
            Task.FromResult(found)

        member _.FindByTypeAsync(accountType: string) =
            let found = !accounts |> List.filter (fun a ->
                match a.AccountType with
                | AccountType.Asset -> accountType = "資産"
                | AccountType.Liability -> accountType = "負債"
                | AccountType.Equity -> accountType = "純資産"
                | AccountType.Revenue -> accountType = "収益"
                | AccountType.Expense -> accountType = "費用")
            Task.FromResult(found)

        member _.SaveAsync(account: Account) =
            let id = nextId
            nextId <- nextId + 1
            let newAccount = { account with AccountId = Some id }
            accounts := newAccount :: !accounts
            Task.FromResult(id)

        member _.UpdateAsync(account: Account) =
            accounts := !accounts |> List.map (fun a ->
                if a.AccountCode.Code = account.AccountCode.Code then account else a)
            Task.FromResult(1)

        member _.DeleteByCodeAsync(accountCode: string) =
            let originalCount = (!accounts).Length
            accounts := !accounts |> List.filter (fun a -> a.AccountCode.Code <> accountCode)
            let deletedCount = originalCount - (!accounts).Length
            Task.FromResult(deletedCount)

/// <summary>
/// AccountService のテスト
/// </summary>
type AccountServiceTest() =

    /// テスト用の勘定科目を作成
    member private _.CreateAccount(code: string, name: string, accountType: AccountType) : Account =
        {
            AccountId = None
            AccountCode = AccountCode.Create(code)
            AccountName = name
            AccountNameKana = Some "テストカナ"
            AccountType = accountType
            IsSummaryAccount = false
            BsplType = Some BsplType.BalanceSheet
            TransactionElementType = Some TransactionElementType.AssetElement
            ExpenseType = None
            DisplayOrder = 10
            IsAggregationTarget = false
            TaxCode = None
            Balance = Money.Zero
        }

    /// テスト用の初期データ
    member private this.CreateInitialAccounts() : Account list =
        [
            { this.CreateAccount("1110", "普通預金", AccountType.Asset) with AccountId = Some 1 }
            { this.CreateAccount("2110", "買掛金", AccountType.Liability) with AccountId = Some 2 }
            { this.CreateAccount("3110", "資本金", AccountType.Equity) with AccountId = Some 3 }
            { this.CreateAccount("4110", "売上高", AccountType.Revenue) with AccountId = Some 4 }
            { this.CreateAccount("5110", "売上原価", AccountType.Expense) with AccountId = Some 5 }
        ]

    [<Fact>]
    member this.``すべての勘定科目を取得できる``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase

            // When
            let! result = service.GetAllAccountsAsync()

            // Then
            result |> should haveLength 5
        }

    [<Fact>]
    member this.``勘定科目コードで検索できる``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase

            // When
            let! result = service.GetAccountByCodeAsync("1110")

            // Then
            result.AccountCode.Code |> should equal "1110"
            result.AccountName |> should equal "普通預金"
        }

    [<Fact>]
    member this.``存在しない勘定科目コードで検索すると例外が発生する``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase

            // When & Then
            let! _ = Assert.ThrowsAsync<AccountNotFoundException>(fun () ->
                service.GetAccountByCodeAsync("9999") :> Task)
            ()
        }

    [<Fact>]
    member this.``勘定科目種別で検索できる``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase

            // When
            let! result = service.GetAccountsByTypeAsync("資産")

            // Then
            result |> should haveLength 1
            result |> List.forall (fun a -> a.AccountType = AccountType.Asset) |> should be True
        }

    [<Fact>]
    member this.``無効な勘定科目種別で検索すると例外が発生する``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase

            // When & Then
            let! _ = Assert.ThrowsAsync<ArgumentException>(fun () ->
                service.GetAccountsByTypeAsync("無効な種別") :> Task)
            ()
        }

    [<Fact>]
    member this.``新しい勘定科目を作成できる``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase
            let newAccount = this.CreateAccount("6110", "給与手当", AccountType.Expense)

            // When
            let! result = service.CreateAccountAsync(newAccount)

            // Then
            result.AccountCode.Code |> should equal "6110"
            result.AccountName |> should equal "給与手当"
            result.AccountId |> should not' (be None)
        }

    [<Fact>]
    member this.``重複する勘定科目コードで作成すると例外が発生する``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase
            let duplicateAccount = this.CreateAccount("1110", "重複科目", AccountType.Asset)

            // When & Then
            let! _ = Assert.ThrowsAsync<DuplicateAccountException>(fun () ->
                service.CreateAccountAsync(duplicateAccount) :> Task)
            ()
        }

    [<Fact>]
    member _.``勘定科目コードが空の場合は例外が発生する``() =
        // Given - AccountCode.Create("") はドメイン型の段階で例外を投げる
        // When & Then
        Assert.Throws<ArgumentException>(fun () ->
            AccountCode.Create("") |> ignore
        ) |> ignore

    [<Fact>]
    member this.``勘定科目名が空の場合は例外が発生する``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase
            let invalidAccount = { this.CreateAccount("9999", "", AccountType.Asset) with AccountName = "" }

            // When & Then
            let! _ = Assert.ThrowsAsync<ArgumentException>(fun () ->
                service.CreateAccountAsync(invalidAccount) :> Task)
            ()
        }

    [<Fact>]
    member this.``勘定科目を更新できる``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase
            let updatedAccount = { this.CreateAccount("1110", "更新後普通預金", AccountType.Asset) with AccountId = Some 1 }

            // When
            let! result = service.UpdateAccountAsync "1110" updatedAccount

            // Then
            result.AccountName |> should equal "更新後普通預金"
        }

    [<Fact>]
    member this.``存在しない勘定科目を更新すると例外が発生する``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase
            let account = this.CreateAccount("9999", "存在しない科目", AccountType.Asset)

            // When & Then
            let! _ = Assert.ThrowsAsync<AccountNotFoundException>(fun () ->
                service.UpdateAccountAsync "9999" account :> Task)
            ()
        }

    [<Fact>]
    member this.``勘定科目を削除できる``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase

            // When
            do! service.DeleteAccountAsync("1110")

            // Then
            let! allAccounts = service.GetAllAccountsAsync()
            allAccounts |> should haveLength 4
            allAccounts |> List.exists (fun a -> a.AccountCode.Code = "1110") |> should be False
        }

    [<Fact>]
    member this.``存在しない勘定科目を削除すると例外が発生する``() =
        task {
            // Given
            let accounts = ref (this.CreateInitialAccounts())
            let repository = MockAccountRepository(accounts)
            let service = AccountService(repository) :> IAccountUseCase

            // When & Then
            let! _ = Assert.ThrowsAsync<AccountNotFoundException>(fun () ->
                service.DeleteAccountAsync("9999") :> Task)
            ()
        }
