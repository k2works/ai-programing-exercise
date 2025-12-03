namespace AccountingSystem.Domain.Models

open AccountingSystem.Domain.Types

/// <summary>
/// 仕訳明細エンティティ
/// </summary>
type JournalDetail = {
    VoucherNumber: VoucherNumber       // 伝票番号
    LineNumber: int                    // 行番号
    AccountCode: AccountCode           // 勘定科目コード
    DebitAmount: Money                 // 借方金額
    CreditAmount: Money                // 貸方金額
    Description: string                // 摘要
    TaxAmount: Money option            // 消費税額
    TaxRate: decimal option            // 消費税率
}

/// JournalDetail エンティティのファクトリ関数とユーティリティ
module JournalDetail =
    /// <summary>
    /// 借方明細を作成
    /// </summary>
    let createDebit voucherNumber lineNumber accountCode amount description =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            LineNumber = lineNumber
            AccountCode = AccountCode.Create(accountCode)
            DebitAmount = amount
            CreditAmount = Money.Zero
            Description = description
            TaxAmount = None
            TaxRate = None
        }

    /// <summary>
    /// 貸方明細を作成
    /// </summary>
    let createCredit voucherNumber lineNumber accountCode amount description =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            LineNumber = lineNumber
            AccountCode = AccountCode.Create(accountCode)
            DebitAmount = Money.Zero
            CreditAmount = amount
            Description = description
            TaxAmount = None
            TaxRate = None
        }

    /// <summary>
    /// エンティティ同一性の判定（VoucherNumber + LineNumber による識別）
    /// </summary>
    let equal (a: JournalDetail) (b: JournalDetail) =
        VoucherNumber.equal a.VoucherNumber b.VoucherNumber &&
        a.LineNumber = b.LineNumber

    /// <summary>
    /// エンティティのハッシュコードを取得
    /// </summary>
    let hashCode (detail: JournalDetail) =
        hash (detail.VoucherNumber.Number, detail.LineNumber)

    /// <summary>
    /// 借方金額かどうか
    /// </summary>
    let isDebit (detail: JournalDetail) =
        detail.DebitAmount.Amount > 0m && detail.CreditAmount.IsZero

    /// <summary>
    /// 貸方金額かどうか
    /// </summary>
    let isCredit (detail: JournalDetail) =
        detail.CreditAmount.Amount > 0m && detail.DebitAmount.IsZero

    /// <summary>
    /// 明細リストの借方合計を計算
    /// </summary>
    let sumDebit (details: JournalDetail list) =
        details |> List.map (fun d -> d.DebitAmount) |> Money.sum

    /// <summary>
    /// 明細リストの貸方合計を計算
    /// </summary>
    let sumCredit (details: JournalDetail list) =
        details |> List.map (fun d -> d.CreditAmount) |> Money.sum

    /// <summary>
    /// 複式簿記の原理を検証（借方合計 = 貸方合計）
    /// </summary>
    let validateBalance (details: JournalDetail list) =
        let debitTotal = sumDebit details
        let creditTotal = sumCredit details
        Money.equal debitTotal creditTotal
