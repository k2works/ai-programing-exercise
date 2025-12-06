namespace AccountingSystem.Domain.Events

open System
open AccountingSystem.Domain.Types

/// <summary>
/// ドメインイベントの共通インターフェース
/// </summary>
type IDomainEvent =
    abstract member AggregateId: string
    abstract member EventType: string
    abstract member EventVersion: int
    abstract member OccurredAt: DateTime
    abstract member UserId: string

/// <summary>
/// 仕訳明細アイテム（イベントソーシング用）
/// </summary>
type JournalEntryLineItem = {
    AccountCode: string
    DebitCredit: DebitCreditType
    Amount: decimal
}

/// <summary>
/// 仕訳関連のドメインイベント
/// </summary>
type JournalEntryEvent =
    | JournalEntryCreated of JournalEntryCreatedData
    | JournalEntryApproved of JournalEntryApprovedData
    | JournalEntryDeleted of JournalEntryDeletedData

and JournalEntryCreatedData = {
    JournalEntryId: string
    EntryDate: DateTime
    Description: string
    LineItems: JournalEntryLineItem list
    UserId: string
    OccurredAt: DateTime
}

and JournalEntryApprovedData = {
    JournalEntryId: string
    ApprovedBy: string
    ApprovalComment: string
    OccurredAt: DateTime
    UserId: string
}

and JournalEntryDeletedData = {
    JournalEntryId: string
    Reason: string
    OccurredAt: DateTime
    UserId: string
}

module JournalEntryEvent =
    /// イベントタイプを取得
    let getEventType = function
        | JournalEntryCreated _ -> "JournalEntryCreated"
        | JournalEntryApproved _ -> "JournalEntryApproved"
        | JournalEntryDeleted _ -> "JournalEntryDeleted"

    /// Aggregate ID を取得
    let getAggregateId = function
        | JournalEntryCreated data -> data.JournalEntryId
        | JournalEntryApproved data -> data.JournalEntryId
        | JournalEntryDeleted data -> data.JournalEntryId

    /// 発生時刻を取得
    let getOccurredAt = function
        | JournalEntryCreated data -> data.OccurredAt
        | JournalEntryApproved data -> data.OccurredAt
        | JournalEntryDeleted data -> data.OccurredAt

    /// User ID を取得
    let getUserId = function
        | JournalEntryCreated data -> data.UserId
        | JournalEntryApproved data -> data.UserId
        | JournalEntryDeleted data -> data.UserId
