namespace FinancialAccounting.Tests.Application

open System
open System.Threading.Tasks
open Xunit
open FinancialAccounting.Domain.Entities
open FinancialAccounting.Application.Ports.In
open FinancialAccounting.Application.Ports.Out
open FinancialAccounting.Application.UseCases

/// <summary>
/// モックリポジトリ
/// </summary>
type MockAccountRepository() =
    let mutable accounts: Account list = []
    let mutable nextId = 1

    interface IAccountRepository with
        member _.SaveAsync(account: Account) =
            task {
                let saved = { account with AccountId = Some nextId }
                nextId <- nextId + 1
                accounts <- accounts @ [saved]
                return saved
            }

        member _.GetByIdAsync(id: int) =
            task {
                return accounts |> List.tryFind (fun a -> a.AccountId = Some id)
            }

        member _.GetByCodeAsync(code: string) =
            task {
                return accounts |> List.tryFind (fun a -> a.AccountCode = code)
            }

        member _.GetAllAsync() =
            task {
                return accounts
            }

        member _.GetByTypeAsync(accountType: AccountType) =
            task {
                return accounts |> List.filter (fun a -> a.AccountType = accountType)
            }

        member _.UpdateAsync(account: Account) =
            task {
                accounts <- accounts |> List.map (fun a ->
                    if a.AccountId = account.AccountId then account else a)
                return account
            }

        member _.DeleteAsync(id: int) =
            task {
                let exists = accounts |> List.exists (fun a -> a.AccountId = Some id)
                if exists then
                    accounts <- accounts |> List.filter (fun a -> a.AccountId <> Some id)
                return exists
            }

module AccountUseCaseTests =

    let createRequest accountCode accountName accountType bsPlType transactionElementType : CreateAccountRequest =
        {
            AccountCode = accountCode
            AccountName = accountName
            AccountNameKana = None
            AccountType = accountType
            IsSummaryAccount = false
            BsPlType = bsPlType
            TransactionElementType = transactionElementType
            ExpenseType = None
            DisplayOrder = 0
            IsAggregationTarget = true
            TaxCode = None
        }

    [<Fact>]
    let ``勘定科目を作成できる`` () =
        task {
            // Arrange
            let repository = MockAccountRepository()
            let useCase = AccountUseCase(repository) :> IAccountUseCase
            let request = createRequest "1110" "現金" "Asset" "B" "Debit"

            // Act
            let! result = useCase.CreateAccountAsync(request)

            // Assert
            match result with
            | Ok account ->
                Assert.Equal("1110", account.AccountCode)
                Assert.Equal("現金", account.AccountName)
                Assert.True(account.AccountId.IsSome)
            | Error msg ->
                Assert.Fail(msg)
        }

    [<Fact>]
    let ``重複する勘定科目コードは作成できない`` () =
        task {
            // Arrange
            let repository = MockAccountRepository()
            let useCase = AccountUseCase(repository) :> IAccountUseCase
            let request = createRequest "1110" "現金" "Asset" "B" "Debit"

            // Act - 1回目の作成
            let! _ = useCase.CreateAccountAsync(request)
            // 2回目の作成（同じコード）
            let! result = useCase.CreateAccountAsync(request)

            // Assert
            match result with
            | Ok _ -> Assert.Fail("エラーが期待されます")
            | Error msg -> Assert.Contains("既に存在", msg)
        }

    [<Fact>]
    let ``無効な勘定科目種別はエラー`` () =
        task {
            // Arrange
            let repository = MockAccountRepository()
            let useCase = AccountUseCase(repository) :> IAccountUseCase
            let request = createRequest "1110" "現金" "InvalidType" "B" "Debit"

            // Act
            let! result = useCase.CreateAccountAsync(request)

            // Assert
            match result with
            | Ok _ -> Assert.Fail("エラーが期待されます")
            | Error msg -> Assert.Contains("無効な勘定科目種別", msg)
        }

    [<Fact>]
    let ``IDで勘定科目を取得できる`` () =
        task {
            // Arrange
            let repository = MockAccountRepository()
            let useCase = AccountUseCase(repository) :> IAccountUseCase
            let request = createRequest "1110" "現金" "Asset" "B" "Debit"
            let! createResult = useCase.CreateAccountAsync(request)
            let accountId =
                match createResult with
                | Ok a -> a.AccountId |> Option.defaultValue 0
                | Error _ -> 0

            // Act
            let! account = useCase.GetAccountByIdAsync(accountId)

            // Assert
            Assert.True(account.IsSome)
            Assert.Equal("1110", account.Value.AccountCode)
        }

    [<Fact>]
    let ``勘定科目コードで勘定科目を取得できる`` () =
        task {
            // Arrange
            let repository = MockAccountRepository()
            let useCase = AccountUseCase(repository) :> IAccountUseCase
            let request = createRequest "1110" "現金" "Asset" "B" "Debit"
            let! _ = useCase.CreateAccountAsync(request)

            // Act
            let! account = useCase.GetAccountByCodeAsync("1110")

            // Assert
            Assert.True(account.IsSome)
            Assert.Equal("現金", account.Value.AccountName)
        }

    [<Fact>]
    let ``全ての勘定科目を取得できる`` () =
        task {
            // Arrange
            let repository = MockAccountRepository()
            let useCase = AccountUseCase(repository) :> IAccountUseCase
            let! _ = useCase.CreateAccountAsync(createRequest "1110" "現金" "Asset" "B" "Debit")
            let! _ = useCase.CreateAccountAsync(createRequest "1120" "売掛金" "Asset" "B" "Debit")

            // Act
            let! accounts = useCase.GetAllAccountsAsync()

            // Assert
            Assert.Equal(2, accounts.Length)
        }

    [<Fact>]
    let ``勘定科目種別で勘定科目を取得できる`` () =
        task {
            // Arrange
            let repository = MockAccountRepository()
            let useCase = AccountUseCase(repository) :> IAccountUseCase
            let! _ = useCase.CreateAccountAsync(createRequest "1110" "現金" "Asset" "B" "Debit")
            let! _ = useCase.CreateAccountAsync(createRequest "4110" "売上高" "Revenue" "P" "Credit")

            // Act
            let! result = useCase.GetAccountsByTypeAsync("Asset")

            // Assert
            match result with
            | Ok accounts ->
                Assert.Equal(1, accounts.Length)
                Assert.Equal("1110", accounts.[0].AccountCode)
            | Error msg ->
                Assert.Fail(msg)
        }

    [<Fact>]
    let ``勘定科目を更新できる`` () =
        task {
            // Arrange
            let repository = MockAccountRepository()
            let useCase = AccountUseCase(repository) :> IAccountUseCase
            let! createResult = useCase.CreateAccountAsync(createRequest "1110" "現金" "Asset" "B" "Debit")
            let accountId =
                match createResult with
                | Ok a -> a.AccountId |> Option.defaultValue 0
                | Error _ -> 0

            let updateRequest: UpdateAccountRequest = {
                AccountId = accountId
                AccountName = "現金預金"
                AccountNameKana = Some "げんきんよきん"
                IsSummaryAccount = false
                ExpenseType = None
                DisplayOrder = 10
                IsAggregationTarget = true
                TaxCode = None
            }

            // Act
            let! result = useCase.UpdateAccountAsync(updateRequest)

            // Assert
            match result with
            | Ok account ->
                Assert.Equal("現金預金", account.AccountName)
                Assert.Equal(10, account.DisplayOrder)
            | Error msg ->
                Assert.Fail(msg)
        }

    [<Fact>]
    let ``勘定科目を削除できる`` () =
        task {
            // Arrange
            let repository = MockAccountRepository()
            let useCase = AccountUseCase(repository) :> IAccountUseCase
            let! createResult = useCase.CreateAccountAsync(createRequest "1110" "現金" "Asset" "B" "Debit")
            let accountId =
                match createResult with
                | Ok a -> a.AccountId |> Option.defaultValue 0
                | Error _ -> 0

            // Act
            let! result = useCase.DeleteAccountAsync(accountId)

            // Assert
            match result with
            | Ok deleted ->
                Assert.True(deleted)
                let! account = useCase.GetAccountByIdAsync(accountId)
                Assert.True(account.IsNone)
            | Error msg ->
                Assert.Fail(msg)
        }
