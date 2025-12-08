namespace FinancialAccounting.Tests.Infrastructure

open System
open Xunit
open FinancialAccounting.Domain.Entities
open FinancialAccounting.Application.Ports.Out
open FinancialAccounting.Infrastructure.Persistence.Repositories

/// <summary>
/// JournalRepository の統合テスト（Testcontainers 使用）
/// </summary>
[<Collection("PostgresCollection")>]
type JournalRepositoryTests(fixture: PostgresTestFixture) =

    let createRepository () : IJournalRepository =
        JournalRepository(fixture.ConnectionString) :> IJournalRepository

    [<Fact>]
    let ``仕訳を保存して取得できる`` () =
        task {
            // Arrange
            let repository = createRepository()
            let journal =
                { Journal.create (DateTime(2024, 1, 15)) "売上計上" 2024 with
                    Entries = [
                        { JournalId = None; AccountCode = "111"; DebitAmount = 1000m; CreditAmount = 0m; Description = "現金" }
                        { JournalId = None; AccountCode = "510"; DebitAmount = 0m; CreditAmount = 1000m; Description = "売上" }
                    ]
                }

            // Act
            let! saved = repository.SaveAsync(journal)
            let! retrieved = repository.GetByIdAsync(saved.JournalId.Value)

            // Assert
            Assert.True(saved.JournalId.IsSome)
            Assert.True(retrieved.IsSome)
            let retrieved = retrieved.Value
            Assert.Equal(saved.JournalId, retrieved.JournalId)
            Assert.Equal(journal.Description, retrieved.Description)
            Assert.Equal(journal.FiscalYear, retrieved.FiscalYear)
            Assert.Equal(2, retrieved.Entries.Length)
        }

    [<Fact>]
    let ``存在しないIDで取得するとNoneを返す`` () =
        task {
            // Arrange
            let repository = createRepository()

            // Act
            let! retrieved = repository.GetByIdAsync(999999)

            // Assert
            Assert.True(retrieved.IsNone)
        }

    [<Fact>]
    let ``会計年度で仕訳一覧を取得できる`` () =
        task {
            // Arrange
            let repository = createRepository()
            let fiscalYear = 2025

            let journal1 =
                { Journal.create (DateTime(2025, 4, 1)) "期首仕訳" fiscalYear with
                    Entries = [
                        { JournalId = None; AccountCode = "111"; DebitAmount = 5000m; CreditAmount = 0m; Description = "現金" }
                        { JournalId = None; AccountCode = "301"; DebitAmount = 0m; CreditAmount = 5000m; Description = "資本金" }
                    ]
                }

            let journal2 =
                { Journal.create (DateTime(2025, 4, 15)) "売上" fiscalYear with
                    Entries = [
                        { JournalId = None; AccountCode = "111"; DebitAmount = 2000m; CreditAmount = 0m; Description = "現金" }
                        { JournalId = None; AccountCode = "510"; DebitAmount = 0m; CreditAmount = 2000m; Description = "売上" }
                    ]
                }

            let! _ = repository.SaveAsync(journal1)
            let! _ = repository.SaveAsync(journal2)

            // Act
            let! journals = repository.GetByFiscalYearAsync(fiscalYear)

            // Assert
            Assert.True(journals.Length >= 2)
            Assert.True(journals |> List.forall (fun j -> j.FiscalYear = fiscalYear))
        }

    [<Fact>]
    let ``仕訳明細の借方・貸方金額が正しく保存される`` () =
        task {
            // Arrange
            let repository = createRepository()
            let journal =
                { Journal.create (DateTime(2024, 6, 1)) "複合仕訳" 2024 with
                    Entries = [
                        { JournalId = None; AccountCode = "111"; DebitAmount = 10000m; CreditAmount = 0m; Description = "現金増加" }
                        { JournalId = None; AccountCode = "211"; DebitAmount = 0m; CreditAmount = 3000m; Description = "買掛金" }
                        { JournalId = None; AccountCode = "510"; DebitAmount = 0m; CreditAmount = 7000m; Description = "売上" }
                    ]
                }

            // Act
            let! saved = repository.SaveAsync(journal)
            let! retrieved = repository.GetByIdAsync(saved.JournalId.Value)

            // Assert
            let retrieved = retrieved.Value
            Assert.Equal(3, retrieved.Entries.Length)

            let debitTotal = retrieved.Entries |> List.sumBy (fun e -> e.DebitAmount)
            let creditTotal = retrieved.Entries |> List.sumBy (fun e -> e.CreditAmount)
            Assert.Equal(10000m, debitTotal)
            Assert.Equal(10000m, creditTotal)
        }
