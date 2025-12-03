module AccountingSystem.Domain.Models.JournalLineItem

open System
open AccountingSystem.Domain.Types

/// <summary>
/// 仕訳貸借明細エンティティ（3層構造の3層目）
/// 借方・貸方の詳細情報を管理
/// </summary>
type JournalLineItem = {
    /// 仕訳伝票番号
    VoucherNumber: VoucherNumber
    /// 仕訳行番号
    LineNumber: int
    /// 仕訳行貸借区分（D: 借方、C: 貸方）
    DebitCreditType: DebitCreditType
    /// 通貨コード
    CurrencyCode: CurrencyCode
    /// 為替レート
    ExchangeRate: decimal
    /// 部門コード
    DepartmentCode: string option
    /// プロジェクトコード
    ProjectCode: string option
    /// 勘定科目コード
    AccountCode: AccountCode
    /// 補助科目コード
    SubAccountCode: string option
    /// 仕訳金額
    Amount: Money
    /// 基軸換算仕訳金額
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
    /// 借方明細を作成
    let createDebit voucherNumber lineNumber accountCode amount description =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            LineNumber = lineNumber
            DebitCreditType = Debit
            CurrencyCode = CurrencyCode.JPY
            ExchangeRate = 1.0m
            DepartmentCode = None
            ProjectCode = None
            AccountCode = AccountCode.Create(accountCode)
            SubAccountCode = None
            Amount = amount
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

    /// 貸方明細を作成
    let createCredit voucherNumber lineNumber accountCode amount description =
        { createDebit voucherNumber lineNumber accountCode amount description with
            DebitCreditType = Credit }

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

    /// 借方合計を計算
    let sumDebit (items: JournalLineItem list) =
        items
        |> List.filter isDebit
        |> List.map (fun i -> i.Amount)
        |> List.fold (+) Money.Zero

    /// 貸方合計を計算
    let sumCredit (items: JournalLineItem list) =
        items
        |> List.filter isCredit
        |> List.map (fun i -> i.Amount)
        |> List.fold (+) Money.Zero

    /// 貸借バランスを検証（複式簿記の原理）
    let validateBalance (items: JournalLineItem list) =
        let debitTotal = sumDebit items
        let creditTotal = sumCredit items
        Money.equal debitTotal creditTotal
