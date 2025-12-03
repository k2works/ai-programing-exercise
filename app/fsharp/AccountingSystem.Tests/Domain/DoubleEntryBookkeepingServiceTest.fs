module AccountingSystem.Tests.Domain.DoubleEntryBookkeepingServiceTest

open Xunit
open AccountingSystem.Domain.Services.DoubleEntryBookkeepingService

module DoubleEntryCheckResultTest =

    [<Fact>]
    let ``Valid 結果は有効である`` () =
        let result = Valid
        Assert.True(DoubleEntryCheckResult.isValid result)

    [<Fact>]
    let ``Invalid 結果は無効である`` () =
        let inconsistencies = [
            { VoucherNumber = "J001"; DebitTotal = 1000m; CreditTotal = 900m; Difference = 100m }
        ]
        let result = Invalid inconsistencies
        Assert.False(DoubleEntryCheckResult.isValid result)

    [<Fact>]
    let ``Valid 結果から不整合リストを取得すると空リスト`` () =
        let result = Valid
        let inconsistencies = DoubleEntryCheckResult.getInconsistencies result
        Assert.Empty(inconsistencies)

    [<Fact>]
    let ``Invalid 結果から不整合リストを取得できる`` () =
        let journals = [
            { VoucherNumber = "J001"; DebitTotal = 1000m; CreditTotal = 900m; Difference = 100m }
            { VoucherNumber = "J002"; DebitTotal = 500m; CreditTotal = 600m; Difference = -100m }
        ]
        let result = Invalid journals
        let inconsistencies = DoubleEntryCheckResult.getInconsistencies result
        Assert.Equal(2, List.length inconsistencies)

    [<Fact>]
    let ``Valid 結果のメッセージを取得できる`` () =
        let result = Valid
        let message = DoubleEntryCheckResult.toMessage result
        Assert.Contains("すべての仕訳", message)
        Assert.Contains("複式簿記", message)

    [<Fact>]
    let ``Invalid 結果のメッセージに不整合件数が含まれる`` () =
        let journals = [
            { VoucherNumber = "J001"; DebitTotal = 1000m; CreditTotal = 900m; Difference = 100m }
            { VoucherNumber = "J002"; DebitTotal = 500m; CreditTotal = 600m; Difference = -100m }
        ]
        let result = Invalid journals
        let message = DoubleEntryCheckResult.toMessage result
        Assert.Contains("2 件", message)
        Assert.Contains("不整合", message)

module InconsistentJournalTest =

    [<Fact>]
    let ``InconsistentJournal を作成できる`` () =
        let journal = {
            VoucherNumber = "J001"
            DebitTotal = 1000m
            CreditTotal = 900m
            Difference = 100m
        }

        Assert.Equal("J001", journal.VoucherNumber)
        Assert.Equal(1000m, journal.DebitTotal)
        Assert.Equal(900m, journal.CreditTotal)
        Assert.Equal(100m, journal.Difference)

    [<Fact>]
    let ``Difference は DebitTotal - CreditTotal で計算される想定`` () =
        let journal = {
            VoucherNumber = "J001"
            DebitTotal = 1500m
            CreditTotal = 1200m
            Difference = 300m  // 1500 - 1200 = 300
        }

        Assert.Equal(journal.DebitTotal - journal.CreditTotal, journal.Difference)
