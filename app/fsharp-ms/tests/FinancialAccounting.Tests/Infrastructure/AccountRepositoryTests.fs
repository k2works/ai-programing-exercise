namespace FinancialAccounting.Tests.Infrastructure

open System
open System.Threading.Tasks
open Xunit
open FinancialAccounting.Domain.Entities
open FinancialAccounting.Application.Ports.Out
open FinancialAccounting.Infrastructure.Persistence.Repositories

/// <summary>
/// AccountRepository のインテグレーションテスト
/// </summary>
[<Collection("PostgresCollection")>]
type AccountRepositoryTests(fixture: PostgresTestFixture) =

    let createTestAccount code name : Account =
        {
            AccountId = None
            AccountCode = code
            AccountName = name
            AccountNameKana = Some "てすと"
            AccountType = Asset
            IsSummaryAccount = false
            BsPlType = BalanceSheet
            TransactionElementType = Debit
            ExpenseType = None
            DisplayOrder = 0
            IsAggregationTarget = true
            TaxCode = None
            Balance = 0m
            CreatedAt = DateTime.UtcNow
            UpdatedAt = DateTime.UtcNow
        }

    [<Fact>]
    member _.``勘定科目を保存して取得できる``() =
        task {
            // Arrange
            let repository = AccountRepository(fixture.ConnectionString) :> IAccountRepository
            let account = createTestAccount "TEST001" "テスト勘定"

            // Act
            let! saved = repository.SaveAsync(account)
            let! retrieved = repository.GetByIdAsync(saved.AccountId.Value)

            // Assert
            Assert.True(retrieved.IsSome)
            Assert.Equal("TEST001", retrieved.Value.AccountCode)
            Assert.Equal("テスト勘定", retrieved.Value.AccountName)
        }

    [<Fact>]
    member _.``勘定科目コードで取得できる``() =
        task {
            // Arrange
            let repository = AccountRepository(fixture.ConnectionString) :> IAccountRepository
            let account = createTestAccount "TEST002" "テスト勘定2"

            // Act
            let! _ = repository.SaveAsync(account)
            let! retrieved = repository.GetByCodeAsync("TEST002")

            // Assert
            Assert.True(retrieved.IsSome)
            Assert.Equal("テスト勘定2", retrieved.Value.AccountName)
        }

    [<Fact>]
    member _.``全ての勘定科目を取得できる``() =
        task {
            // Arrange
            let repository = AccountRepository(fixture.ConnectionString) :> IAccountRepository
            let! _ = repository.SaveAsync(createTestAccount "TEST003" "テスト勘定3")
            let! _ = repository.SaveAsync(createTestAccount "TEST004" "テスト勘定4")

            // Act
            let! accounts = repository.GetAllAsync()

            // Assert
            Assert.True(accounts.Length >= 2)
        }

    [<Fact>]
    member _.``勘定科目種別で取得できる``() =
        task {
            // Arrange
            let repository = AccountRepository(fixture.ConnectionString) :> IAccountRepository
            let assetAccount = createTestAccount "TEST005" "資産勘定"
            let revenueAccount = { createTestAccount "TEST006" "収益勘定" with
                                        AccountType = Revenue
                                        BsPlType = ProfitAndLoss
                                        TransactionElementType = Credit }

            let! _ = repository.SaveAsync(assetAccount)
            let! _ = repository.SaveAsync(revenueAccount)

            // Act
            let! assetAccounts = repository.GetByTypeAsync(Asset)

            // Assert
            Assert.True(assetAccounts |> List.exists (fun a -> a.AccountCode = "TEST005"))
        }

    [<Fact>]
    member _.``勘定科目を更新できる``() =
        task {
            // Arrange
            let repository = AccountRepository(fixture.ConnectionString) :> IAccountRepository
            let account = createTestAccount "TEST007" "更新前"
            let! saved = repository.SaveAsync(account)

            // Act
            let updated = { saved with AccountName = "更新後"; DisplayOrder = 100 }
            let! result = repository.UpdateAsync(updated)
            let! retrieved = repository.GetByIdAsync(saved.AccountId.Value)

            // Assert
            Assert.Equal("更新後", retrieved.Value.AccountName)
            Assert.Equal(100, retrieved.Value.DisplayOrder)
        }

    [<Fact>]
    member _.``勘定科目を削除できる``() =
        task {
            // Arrange
            let repository = AccountRepository(fixture.ConnectionString) :> IAccountRepository
            let account = createTestAccount "TEST008" "削除対象"
            let! saved = repository.SaveAsync(account)

            // Act
            let! deleted = repository.DeleteAsync(saved.AccountId.Value)
            let! retrieved = repository.GetByIdAsync(saved.AccountId.Value)

            // Assert
            Assert.True(deleted)
            Assert.True(retrieved.IsNone)
        }

    [<Fact>]
    member _.``存在しないIDで取得するとNoneを返す``() =
        task {
            // Arrange
            let repository = AccountRepository(fixture.ConnectionString) :> IAccountRepository

            // Act
            let! result = repository.GetByIdAsync(99999)

            // Assert
            Assert.True(result.IsNone)
        }
