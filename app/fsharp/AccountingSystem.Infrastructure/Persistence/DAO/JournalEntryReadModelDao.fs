namespace AccountingSystem.Infrastructure.Persistence.DAO

open System

/// <summary>
/// 仕訳 Read Model エンティティ（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type JournalEntryReadModelEntity = {
    id: string
    entry_date: DateTime
    description: string
    status: string
    deleted: bool
    created_at: DateTime
    updated_at: DateTime
    approved_by: string
    approval_comment: string
}

/// <summary>
/// 仕訳明細 Read Model エンティティ（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type JournalEntryLineReadModelEntity = {
    id: int64
    journal_entry_id: string
    account_code: string
    debit_credit: string
    amount: decimal
}
