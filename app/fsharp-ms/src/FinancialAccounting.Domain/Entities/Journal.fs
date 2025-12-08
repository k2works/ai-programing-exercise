namespace FinancialAccounting.Domain.Entities

open System

/// <summary>
/// 仕訳明細エンティティ
/// </summary>
type JournalEntry = {
    JournalId: int option
    AccountCode: string
    DebitAmount: decimal
    CreditAmount: decimal
    Description: string
}

/// <summary>
/// 仕訳エンティティ
/// </summary>
type Journal = {
    JournalId: int option
    JournalDate: DateTime
    Description: string
    FiscalYear: int
    Entries: JournalEntry list
}

module Journal =
    /// <summary>
    /// 新しい仕訳を作成
    /// </summary>
    let create (journalDate: DateTime) (description: string) (fiscalYear: int) : Journal =
        {
            JournalId = None
            JournalDate = journalDate
            Description = description
            FiscalYear = fiscalYear
            Entries = []
        }

    /// <summary>
    /// 仕訳明細を追加（ビジネスルール: 借方・貸方の検証）
    /// </summary>
    let addEntry (entry: JournalEntry) (journal: Journal) : Result<Journal, string> =
        if entry.DebitAmount = 0m && entry.CreditAmount = 0m then
            Error "借方・貸方の少なくとも一方は0より大きい必要があります"
        else
            Ok { journal with Entries = journal.Entries @ [entry] }

    /// <summary>
    /// 借方合計と貸方合計が一致することを検証
    /// </summary>
    let validateBalance (journal: Journal) : Result<unit, string> =
        let debitTotal = journal.Entries |> List.sumBy (fun e -> e.DebitAmount)
        let creditTotal = journal.Entries |> List.sumBy (fun e -> e.CreditAmount)

        if debitTotal <> creditTotal then
            Error $"貸借が一致しません。借方合計: {debitTotal}, 貸方合計: {creditTotal}"
        else
            Ok ()
