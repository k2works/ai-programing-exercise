module AccountingSystem.Tests.Domain.JournalLineItemTest

open AccountingSystem.Domain.Models.JournalLineItem
open AccountingSystem.Domain.Types
open Xunit
open FsUnit.Xunit

/// <summary>
/// JournalLineItem エンティティのテスト（3層構造3層目）
/// </summary>
type JournalLineItemTest() =

    [<Fact>]
    member _.``JournalLineItem.createDebit で借方明細を作成できる``() =
        let item = JournalLineItem.createDebit "JN240001" 1 "1100" (Money.Create(110000m)) "現金入金"

        item.VoucherNumber.Number |> should equal "JN240001"
        item.LineNumber |> should equal 1
        item.DebitCreditType |> should equal Debit
        item.AccountCode.Code |> should equal "1100"
        item.Amount.Amount |> should equal 110000m
        item.Amount.Currency.Code |> should equal "JPY"
        item.BaseAmount.Amount |> should equal 110000m
        item.ExchangeRate |> should equal 1.0m
        JournalLineItem.isDebit item |> should equal true
        JournalLineItem.isCredit item |> should equal false

    [<Fact>]
    member _.``JournalLineItem.createCredit で貸方明細を作成できる``() =
        let item = JournalLineItem.createCredit "JN240001" 1 "4100" (Money.Create(100000m)) "売上"

        item.VoucherNumber.Number |> should equal "JN240001"
        item.LineNumber |> should equal 1
        item.DebitCreditType |> should equal Credit
        item.AccountCode.Code |> should equal "4100"
        item.Amount.Amount |> should equal 100000m
        JournalLineItem.isDebit item |> should equal false
        JournalLineItem.isCredit item |> should equal true

    [<Fact>]
    member _.``同じ VoucherNumber, LineNumber, DebitCreditType を持つ JournalLineItem は同一エンティティである``() =
        let item1 = JournalLineItem.createDebit "JN240001" 1 "1100" (Money.Create(110000m)) "現金入金"
        let item2 = { item1 with Amount = CurrencyAmount.CreateJPY(200000m); BaseAmount = Money.Create(200000m) }

        JournalLineItem.equal item1 item2 |> should equal true

    [<Fact>]
    member _.``異なる DebitCreditType を持つ JournalLineItem は異なるエンティティである``() =
        let item1 = JournalLineItem.createDebit "JN240001" 1 "1100" (Money.Create(110000m)) "現金入金"
        let item2 = JournalLineItem.createCredit "JN240001" 1 "4100" (Money.Create(110000m)) "売上"

        JournalLineItem.equal item1 item2 |> should equal false

    [<Fact>]
    member _.``JournalLineItem.sumDebit で借方合計を計算できる``() =
        let items = [
            JournalLineItem.createDebit "JN240001" 1 "1100" (Money.Create(100000m)) "現金"
            JournalLineItem.createDebit "JN240001" 1 "6200" (Money.Create(500m)) "手数料"
            JournalLineItem.createCredit "JN240001" 1 "1300" (Money.Create(100500m)) "売掛金"
        ]

        let debitTotal = JournalLineItem.sumDebit items
        debitTotal.Amount |> should equal 100500m

    [<Fact>]
    member _.``JournalLineItem.sumCredit で貸方合計を計算できる``() =
        let items = [
            JournalLineItem.createDebit "JN240001" 1 "1100" (Money.Create(100000m)) "現金"
            JournalLineItem.createDebit "JN240001" 1 "6200" (Money.Create(500m)) "手数料"
            JournalLineItem.createCredit "JN240001" 1 "1300" (Money.Create(100500m)) "売掛金"
        ]

        let creditTotal = JournalLineItem.sumCredit items
        creditTotal.Amount |> should equal 100500m

    [<Fact>]
    member _.``JournalLineItem.validateBalance で複式簿記の原理を検証できる（貸借一致）``() =
        let items = [
            JournalLineItem.createDebit "JN240001" 1 "1100" (Money.Create(110000m)) "現金"
            JournalLineItem.createCredit "JN240001" 1 "4100" (Money.Create(100000m)) "売上"
            JournalLineItem.createCredit "JN240001" 1 "2120" (Money.Create(10000m)) "消費税"
        ]

        JournalLineItem.validateBalance items |> should equal true

    [<Fact>]
    member _.``JournalLineItem.validateBalance で複式簿記の原理を検証できる（貸借不一致）``() =
        let items = [
            JournalLineItem.createDebit "JN240001" 1 "1100" (Money.Create(110000m)) "現金"
            JournalLineItem.createCredit "JN240001" 1 "4100" (Money.Create(100000m)) "売上"
            // 消費税が不足
        ]

        JournalLineItem.validateBalance items |> should equal false

    [<Fact>]
    member _.``複合仕訳（売掛金回収と振込手数料）の貸借が一致する``() =
        // 借方：普通預金 104,500（振込手数料差引後）
        // 借方：支払手数料 500
        // 貸方：売掛金 105,000
        let items = [
            JournalLineItem.createDebit "JN240002" 1 "1200" (Money.Create(104500m)) "売掛金回収（手数料差引後）"
            JournalLineItem.createDebit "JN240002" 1 "6200" (Money.Create(500m)) "振込手数料"
            JournalLineItem.createCredit "JN240002" 1 "1300" (Money.Create(105000m)) "売掛金回収"
        ]

        JournalLineItem.validateBalance items |> should equal true
        JournalLineItem.sumDebit items |> fun m -> m.Amount |> should equal 105000m
        JournalLineItem.sumCredit items |> fun m -> m.Amount |> should equal 105000m
