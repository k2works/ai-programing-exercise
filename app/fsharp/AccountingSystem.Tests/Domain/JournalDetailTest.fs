module AccountingSystem.Tests.Domain.JournalDetailTest

open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Types
open Xunit
open FsUnit.Xunit

/// <summary>
/// JournalDetail エンティティのテスト
/// </summary>
type JournalDetailTest() =

    [<Fact>]
    member _.``JournalDetail.createDebit で借方明細を作成できる``() =
        let detail = JournalDetail.createDebit "JE240001" 1 "1100" (Money.Create(110000m)) "現金入金"

        detail.VoucherNumber.Number |> should equal "JE240001"
        detail.LineNumber |> should equal 1
        detail.AccountCode.Code |> should equal "1100"
        detail.DebitAmount.Amount |> should equal 110000m
        detail.CreditAmount.Amount |> should equal 0m
        detail.Description |> should equal "現金入金"
        JournalDetail.isDebit detail |> should equal true
        JournalDetail.isCredit detail |> should equal false

    [<Fact>]
    member _.``JournalDetail.createCredit で貸方明細を作成できる``() =
        let detail = JournalDetail.createCredit "JE240001" 2 "4100" (Money.Create(100000m)) "売上"

        detail.VoucherNumber.Number |> should equal "JE240001"
        detail.LineNumber |> should equal 2
        detail.AccountCode.Code |> should equal "4100"
        detail.DebitAmount.Amount |> should equal 0m
        detail.CreditAmount.Amount |> should equal 100000m
        detail.Description |> should equal "売上"
        JournalDetail.isDebit detail |> should equal false
        JournalDetail.isCredit detail |> should equal true

    [<Fact>]
    member _.``同じ VoucherNumber と LineNumber を持つ JournalDetail は同一エンティティである``() =
        let detail1 = JournalDetail.createDebit "JE240001" 1 "1100" (Money.Create(110000m)) "現金入金"
        let detail2 = { detail1 with DebitAmount = Money.Create(200000m); Description = "変更後" }

        JournalDetail.equal detail1 detail2 |> should equal true

    [<Fact>]
    member _.``異なる LineNumber を持つ JournalDetail は異なるエンティティである``() =
        let detail1 = JournalDetail.createDebit "JE240001" 1 "1100" (Money.Create(110000m)) "現金入金"
        let detail2 = JournalDetail.createDebit "JE240001" 2 "1100" (Money.Create(110000m)) "現金入金"

        JournalDetail.equal detail1 detail2 |> should equal false

    [<Fact>]
    member _.``JournalDetail.sumDebit で借方合計を計算できる``() =
        let details = [
            JournalDetail.createDebit "JE240001" 1 "1100" (Money.Create(100000m)) "現金"
            JournalDetail.createDebit "JE240001" 2 "6200" (Money.Create(500m)) "手数料"
            JournalDetail.createCredit "JE240001" 3 "1300" (Money.Create(100500m)) "売掛金"
        ]

        let debitTotal = JournalDetail.sumDebit details
        debitTotal.Amount |> should equal 100500m

    [<Fact>]
    member _.``JournalDetail.sumCredit で貸方合計を計算できる``() =
        let details = [
            JournalDetail.createDebit "JE240001" 1 "1100" (Money.Create(100000m)) "現金"
            JournalDetail.createDebit "JE240001" 2 "6200" (Money.Create(500m)) "手数料"
            JournalDetail.createCredit "JE240001" 3 "1300" (Money.Create(100500m)) "売掛金"
        ]

        let creditTotal = JournalDetail.sumCredit details
        creditTotal.Amount |> should equal 100500m

    [<Fact>]
    member _.``JournalDetail.validateBalance で複式簿記の原理を検証できる（貸借一致）``() =
        let details = [
            JournalDetail.createDebit "JE240001" 1 "1100" (Money.Create(110000m)) "現金"
            JournalDetail.createCredit "JE240001" 2 "4100" (Money.Create(100000m)) "売上"
            JournalDetail.createCredit "JE240001" 3 "2120" (Money.Create(10000m)) "消費税"
        ]

        JournalDetail.validateBalance details |> should equal true

    [<Fact>]
    member _.``JournalDetail.validateBalance で複式簿記の原理を検証できる（貸借不一致）``() =
        let details = [
            JournalDetail.createDebit "JE240001" 1 "1100" (Money.Create(110000m)) "現金"
            JournalDetail.createCredit "JE240001" 2 "4100" (Money.Create(100000m)) "売上"
            // 消費税が不足
        ]

        JournalDetail.validateBalance details |> should equal false

    [<Fact>]
    member _.``複雑な仕訳（売掛金回収と振込手数料）の貸借が一致する``() =
        // 借方：普通預金 104,500（振込手数料差引後）
        // 借方：支払手数料 500
        // 貸方：売掛金 105,000
        let details = [
            JournalDetail.createDebit "JE240002" 1 "1200" (Money.Create(104500m)) "売掛金回収（手数料差引後）"
            JournalDetail.createDebit "JE240002" 2 "6200" (Money.Create(500m)) "振込手数料"
            JournalDetail.createCredit "JE240002" 3 "1300" (Money.Create(105000m)) "売掛金回収"
        ]

        JournalDetail.validateBalance details |> should equal true
        JournalDetail.sumDebit details |> fun m -> m.Amount |> should equal 105000m
        JournalDetail.sumCredit details |> fun m -> m.Amount |> should equal 105000m
