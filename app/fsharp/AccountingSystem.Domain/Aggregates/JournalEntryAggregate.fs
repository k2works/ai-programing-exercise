namespace AccountingSystem.Domain.Aggregates

open System
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events

/// <summary>
/// 仕訳 Aggregate の状態
/// </summary>
type JournalEntryAggregate = {
    Id: string
    EntryDate: DateTime
    Description: string
    LineItems: JournalEntryLineItem list
    Status: JournalEntryStatus
    Deleted: bool
    Version: int
    UncommittedEvents: JournalEntryEvent list
}

module JournalEntryAggregate =
    /// <summary>
    /// 空の Aggregate を作成
    /// </summary>
    let empty : JournalEntryAggregate = {
        Id = ""
        EntryDate = DateTime.MinValue
        Description = ""
        LineItems = []
        Status = JournalEntryStatus.Draft
        Deleted = false
        Version = 0
        UncommittedEvents = []
    }

    /// <summary>
    /// イベント適用（状態遷移）
    /// </summary>
    let applyEvent (aggregate: JournalEntryAggregate) (event: JournalEntryEvent) : JournalEntryAggregate =
        match event with
        | JournalEntryCreated data ->
            { aggregate with
                Id = data.JournalEntryId
                EntryDate = data.EntryDate
                Description = data.Description
                LineItems = data.LineItems
                Status = JournalEntryStatus.Draft
            }

        | JournalEntryApproved _ ->
            { aggregate with Status = JournalEntryStatus.Approved }

        | JournalEntryDeleted _ ->
            { aggregate with Deleted = true }

    /// <summary>
    /// イベント再生（fold で状態を復元）
    /// </summary>
    let replay (events: JournalEntryEvent list) : JournalEntryAggregate =
        events
        |> List.fold (fun aggregate event ->
            { (applyEvent aggregate event) with Version = aggregate.Version + 1 }
        ) empty

    /// <summary>
    /// スナップショットからイベント再生（最適化用）
    /// </summary>
    let replayFrom (snapshot: JournalEntryAggregate) (events: JournalEntryEvent list) : JournalEntryAggregate =
        events
        |> List.fold (fun aggregate event ->
            { (applyEvent aggregate event) with Version = aggregate.Version + 1 }
        ) snapshot

    /// <summary>
    /// ビジネスルール検証: 借方・貸方の合計一致
    /// </summary>
    let validateBalance (lineItems: JournalEntryLineItem list) : Result<unit, string> =
        let debitTotal =
            lineItems
            |> List.filter (fun item -> item.DebitCredit = DebitCreditType.Debit)
            |> List.sumBy (fun item -> item.Amount)

        let creditTotal =
            lineItems
            |> List.filter (fun item -> item.DebitCredit = DebitCreditType.Credit)
            |> List.sumBy (fun item -> item.Amount)

        if debitTotal <> creditTotal then
            Error $"借方合計と貸方合計が一致しません: 借方={debitTotal}, 貸方={creditTotal}"
        else
            Ok ()

    /// <summary>
    /// コマンド: 仕訳を作成
    /// </summary>
    let create
        (id: string)
        (entryDate: DateTime)
        (description: string)
        (lineItems: JournalEntryLineItem list)
        (userId: string)
        : Result<JournalEntryAggregate, string> =

        // ビジネスルール検証
        if List.isEmpty lineItems then
            Error "仕訳明細が必要です"
        else
            match validateBalance lineItems with
            | Error msg -> Error msg
            | Ok () ->
                // イベント発行
                let event = JournalEntryCreated {
                    JournalEntryId = id
                    EntryDate = entryDate
                    Description = description
                    LineItems = lineItems
                    UserId = userId
                    OccurredAt = DateTime.UtcNow
                }

                let aggregate = applyEvent empty event

                Ok { aggregate with
                        Version = 1
                        UncommittedEvents = [event] }

    /// <summary>
    /// コマンド: 仕訳を承認
    /// </summary>
    let approve
        (approvedBy: string)
        (approvalComment: string)
        (aggregate: JournalEntryAggregate)
        : Result<JournalEntryAggregate, string> =

        if aggregate.Deleted then
            Error "削除済みの仕訳は承認できません"
        elif aggregate.Status = JournalEntryStatus.Approved then
            Error "すでに承認済みです"
        else
            let event = JournalEntryApproved {
                JournalEntryId = aggregate.Id
                ApprovedBy = approvedBy
                ApprovalComment = approvalComment
                OccurredAt = DateTime.UtcNow
                UserId = approvedBy
            }

            let updatedAggregate = applyEvent aggregate event

            Ok { updatedAggregate with
                    Version = aggregate.Version + 1
                    UncommittedEvents = aggregate.UncommittedEvents @ [event] }

    /// <summary>
    /// コマンド: 仕訳を削除
    /// </summary>
    let delete
        (reason: string)
        (userId: string)
        (aggregate: JournalEntryAggregate)
        : Result<JournalEntryAggregate, string> =

        if aggregate.Deleted then
            Error "すでに削除済みです"
        else
            let event = JournalEntryDeleted {
                JournalEntryId = aggregate.Id
                Reason = reason
                OccurredAt = DateTime.UtcNow
                UserId = userId
            }

            let updatedAggregate = applyEvent aggregate event

            Ok { updatedAggregate with
                    Version = aggregate.Version + 1
                    UncommittedEvents = aggregate.UncommittedEvents @ [event] }

    /// <summary>
    /// 未コミットイベントをクリア
    /// </summary>
    let markEventsAsCommitted (aggregate: JournalEntryAggregate) : JournalEntryAggregate =
        { aggregate with UncommittedEvents = [] }

    /// <summary>
    /// 借方合計を計算
    /// </summary>
    let sumDebit (aggregate: JournalEntryAggregate) : decimal =
        aggregate.LineItems
        |> List.filter (fun item -> item.DebitCredit = DebitCreditType.Debit)
        |> List.sumBy (fun item -> item.Amount)

    /// <summary>
    /// 貸方合計を計算
    /// </summary>
    let sumCredit (aggregate: JournalEntryAggregate) : decimal =
        aggregate.LineItems
        |> List.filter (fun item -> item.DebitCredit = DebitCreditType.Credit)
        |> List.sumBy (fun item -> item.Amount)
