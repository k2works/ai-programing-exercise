module AccountingSystem.Domain.Models.JournalLineItem

open System
open AccountingSystem.Domain.Types

/// <summary>
/// 仕訳貸借明細エンティティ（3層構造の3層目）
/// 借方・貸方の詳細情報を管理
/// Amount は取引通貨建て、BaseAmount は基軸通貨（日本円）建て
/// </summary>
type JournalLineItem = {
    /// 仕訳伝票番号
    VoucherNumber: VoucherNumber
    /// 仕訳行番号
    LineNumber: int
    /// 仕訳行貸借区分（D: 借方、C: 貸方）
    DebitCreditType: DebitCreditType
    /// 為替レート（取引通貨→日本円）
    ExchangeRate: decimal
    /// 部門コード
    DepartmentCode: string option
    /// プロジェクトコード
    ProjectCode: string option
    /// 勘定科目コード
    AccountCode: AccountCode
    /// 補助科目コード
    SubAccountCode: string option
    /// 仕訳金額（取引通貨建て）
    Amount: CurrencyAmount
    /// 基軸換算仕訳金額（日本円建て）
    BaseAmount: Money
    /// 消費税区分
    TaxCategory: string option
    /// 消費税率
    TaxRate: int option
    /// 消費税計算区分
    TaxCalculationType: string option
    /// 期日（支払期日・入金期日）
    DueDate: DateTime option
    /// 資金繰フラグ
    IsCashFlow: bool
    /// セグメントコード
    SegmentCode: string option
    /// 相手勘定科目コード
    CounterAccountCode: string option
    /// 相手補助科目コード
    CounterSubAccountCode: string option
    /// 付箋コード
    MemoCode: string option
    /// 付箋内容
    MemoContent: string option
    /// 作成日時
    CreatedAt: DateTime
    /// 更新日時
    UpdatedAt: DateTime
}

module JournalLineItem =
    /// 借方明細を作成（日本円）
    let createDebit voucherNumber lineNumber accountCode (amount: Money) description =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            LineNumber = lineNumber
            DebitCreditType = Debit
            ExchangeRate = 1.0m
            DepartmentCode = None
            ProjectCode = None
            AccountCode = AccountCode.Create(accountCode)
            SubAccountCode = None
            Amount = CurrencyAmount.CreateJPY(amount.Amount)
            BaseAmount = amount
            TaxCategory = None
            TaxRate = None
            TaxCalculationType = None
            DueDate = None
            IsCashFlow = false
            SegmentCode = None
            CounterAccountCode = None
            CounterSubAccountCode = None
            MemoCode = None
            MemoContent = None
            CreatedAt = DateTime.UtcNow
            UpdatedAt = DateTime.UtcNow
        }

    /// 借方明細を作成（外貨）
    let createDebitForeign voucherNumber lineNumber accountCode (amount: CurrencyAmount) (baseAmount: Money) exchangeRate description =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            LineNumber = lineNumber
            DebitCreditType = Debit
            ExchangeRate = exchangeRate
            DepartmentCode = None
            ProjectCode = None
            AccountCode = AccountCode.Create(accountCode)
            SubAccountCode = None
            Amount = amount
            BaseAmount = baseAmount
            TaxCategory = None
            TaxRate = None
            TaxCalculationType = None
            DueDate = None
            IsCashFlow = false
            SegmentCode = None
            CounterAccountCode = None
            CounterSubAccountCode = None
            MemoCode = None
            MemoContent = None
            CreatedAt = DateTime.UtcNow
            UpdatedAt = DateTime.UtcNow
        }

    /// 貸方明細を作成（日本円）
    let createCredit voucherNumber lineNumber accountCode (amount: Money) description =
        { createDebit voucherNumber lineNumber accountCode amount description with
            DebitCreditType = Credit }

    /// 貸方明細を作成（外貨）
    let createCreditForeign voucherNumber lineNumber accountCode (amount: CurrencyAmount) (baseAmount: Money) exchangeRate description =
        { createDebitForeign voucherNumber lineNumber accountCode amount baseAmount exchangeRate description with
            DebitCreditType = Credit }

    /// 通貨コードを取得
    let currencyCode (item: JournalLineItem) =
        item.Amount.Currency

    /// エンティティの同一性判定（VoucherNumber + LineNumber + DebitCreditType で判定）
    let equal (a: JournalLineItem) (b: JournalLineItem) =
        a.VoucherNumber.Number = b.VoucherNumber.Number &&
        a.LineNumber = b.LineNumber &&
        a.DebitCreditType = b.DebitCreditType

    /// ハッシュコード
    let hashCode (item: JournalLineItem) =
        hash (item.VoucherNumber.Number, item.LineNumber, item.DebitCreditType)

    /// 借方かどうか
    let isDebit (item: JournalLineItem) =
        item.DebitCreditType = Debit

    /// 貸方かどうか
    let isCredit (item: JournalLineItem) =
        item.DebitCreditType = Credit

    /// 借方合計を計算（基軸通貨=日本円建て）
    let sumDebit (items: JournalLineItem list) =
        items
        |> List.filter isDebit
        |> List.map (fun i -> i.BaseAmount)
        |> List.fold (+) Money.Zero

    /// 貸方合計を計算（基軸通貨=日本円建て）
    let sumCredit (items: JournalLineItem list) =
        items
        |> List.filter isCredit
        |> List.map (fun i -> i.BaseAmount)
        |> List.fold (+) Money.Zero

    /// 貸借バランスを検証（複式簿記の原理、基軸通貨=日本円建てで検証）
    let validateBalance (items: JournalLineItem list) =
        let debitTotal = sumDebit items
        let creditTotal = sumCredit items
        Money.equal debitTotal creditTotal
