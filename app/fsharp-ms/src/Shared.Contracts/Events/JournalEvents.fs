namespace Shared.Contracts.Events

open System

/// <summary>
/// 仕訳が作成されたときに発行されるイベント
/// </summary>
type JournalCreatedEvent = {
    JournalId: int
    FiscalYear: int
    JournalDate: DateTime
    Description: string
    TotalAmount: decimal
    CreatedAt: DateTime
}

/// <summary>
/// 仕訳明細の情報
/// </summary>
type JournalEntryInfo = {
    AccountCode: string
    DebitAmount: decimal
    CreditAmount: decimal
    Description: string
}

/// <summary>
/// 仕訳が更新されたときに発行されるイベント
/// </summary>
type JournalUpdatedEvent = {
    JournalId: int
    FiscalYear: int
    JournalDate: DateTime
    Description: string
    Entries: JournalEntryInfo list
    UpdatedAt: DateTime
}

/// <summary>
/// 仕訳が削除されたときに発行されるイベント
/// </summary>
type JournalDeletedEvent = {
    JournalId: int
    FiscalYear: int
    DeletedAt: DateTime
}
