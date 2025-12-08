namespace FinancialAccounting.Tests.Domain

open System
open Xunit
open FinancialAccounting.Domain.Models

module JournalTests =

    [<Fact>]
    let ``新しい仕訳を作成できる`` () =
        // Arrange
        let journalDate = DateTime(2024, 1, 15)
        let description = "売上計上"
        let fiscalYear = 2024

        // Act
        let journal = Journal.create journalDate description fiscalYear

        // Assert
        Assert.Equal(None, journal.JournalId)
        Assert.Equal(journalDate, journal.JournalDate)
        Assert.Equal(description, journal.Description)
        Assert.Equal(fiscalYear, journal.FiscalYear)
        Assert.Empty(journal.Entries)

    [<Fact>]
    let ``仕訳明細を追加できる`` () =
        // Arrange
        let journal = Journal.create (DateTime(2024, 1, 15)) "売上計上" 2024
        let entry = {
            JournalId = None
            AccountCode = "111"
            DebitAmount = 1000m
            CreditAmount = 0m
            Description = "現金増加"
        }

        // Act
        let result = Journal.addEntry entry journal

        // Assert
        match result with
        | Ok j -> Assert.Single(j.Entries) |> ignore
        | Error _ -> Assert.Fail("エントリー追加に成功するはず")

    [<Fact>]
    let ``借方・貸方が両方0の明細は追加できない`` () =
        // Arrange
        let journal = Journal.create (DateTime(2024, 1, 15)) "テスト" 2024
        let entry = {
            JournalId = None
            AccountCode = "111"
            DebitAmount = 0m
            CreditAmount = 0m
            Description = "無効な明細"
        }

        // Act
        let result = Journal.addEntry entry journal

        // Assert
        match result with
        | Ok _ -> Assert.Fail("エラーになるはず")
        | Error msg -> Assert.Contains("借方・貸方", msg)

    [<Fact>]
    let ``借方合計と貸方合計が一致する場合は検証成功`` () =
        // Arrange
        let journal = Journal.create (DateTime(2024, 1, 15)) "売上計上" 2024
        let entry1 = { JournalId = None; AccountCode = "111"; DebitAmount = 1000m; CreditAmount = 0m; Description = "現金" }
        let entry2 = { JournalId = None; AccountCode = "510"; DebitAmount = 0m; CreditAmount = 1000m; Description = "売上" }

        let journalWithEntries =
            { journal with Entries = [entry1; entry2] }

        // Act
        let result = Journal.validateBalance journalWithEntries

        // Assert
        match result with
        | Ok () -> () // 成功
        | Error msg -> Assert.Fail($"検証に成功するはず: {msg}")

    [<Fact>]
    let ``借方合計と貸方合計が一致しない場合は検証失敗`` () =
        // Arrange
        let journal = Journal.create (DateTime(2024, 1, 15)) "テスト" 2024
        let entry1 = { JournalId = None; AccountCode = "111"; DebitAmount = 1000m; CreditAmount = 0m; Description = "現金" }
        let entry2 = { JournalId = None; AccountCode = "510"; DebitAmount = 0m; CreditAmount = 500m; Description = "売上" }

        let journalWithEntries =
            { journal with Entries = [entry1; entry2] }

        // Act
        let result = Journal.validateBalance journalWithEntries

        // Assert
        match result with
        | Ok () -> Assert.Fail("エラーになるはず")
        | Error msg -> Assert.Contains("貸借が一致しません", msg)
