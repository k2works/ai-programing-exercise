namespace AccountingSystem.Infrastructure.Web.Dtos

open System
open System.ComponentModel.DataAnnotations
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open AccountingSystem.Domain.Aggregates

/// <summary>
/// 仕訳明細項目リクエスト DTO（イベントソーシング用）
/// </summary>
[<CLIMutable>]
type JournalEntryLineItemRequest = {
    [<Required(ErrorMessage = "勘定科目コードは必須です")>]
    AccountCode: string

    [<Required(ErrorMessage = "貸借区分は必須です")>]
    DebitCredit: string

    [<Required(ErrorMessage = "金額は必須です")>]
    [<Range(0.01, 999999999999.99, ErrorMessage = "金額は0より大きい必要があります")>]
    Amount: decimal
}

module JournalEntryLineItemRequest =
    /// Domain Model への変換
    let toDomain (request: JournalEntryLineItemRequest) : JournalEntryLineItem =
        let debitCredit =
            match request.DebitCredit with
            | "D" | "借" | "Debit" -> DebitCreditType.Debit
            | "C" | "貸" | "Credit" -> DebitCreditType.Credit
            | _ -> DebitCreditType.Debit
        {
            AccountCode = request.AccountCode
            DebitCredit = debitCredit
            Amount = request.Amount
        }

/// <summary>
/// 仕訳作成リクエスト DTO（イベントソーシング用）
/// </summary>
[<CLIMutable>]
type CreateJournalEntryRequest = {
    [<Required(ErrorMessage = "仕訳IDは必須です")>]
    Id: string

    [<Required(ErrorMessage = "仕訳日は必須です")>]
    EntryDate: DateTime

    [<Required(ErrorMessage = "摘要は必須です")>]
    Description: string

    [<Required(ErrorMessage = "明細は必須です")>]
    [<MinLength(1, ErrorMessage = "明細が1件以上必要です")>]
    LineItems: JournalEntryLineItemRequest array
}

/// <summary>
/// 仕訳承認リクエスト DTO
/// </summary>
[<CLIMutable>]
type ApproveJournalEntryRequest = {
    [<Required(ErrorMessage = "承認者は必須です")>]
    ApprovedBy: string

    ApprovalComment: string
}

/// <summary>
/// 仕訳削除リクエスト DTO
/// </summary>
[<CLIMutable>]
type DeleteJournalEntryRequest = {
    [<Required(ErrorMessage = "削除理由は必須です")>]
    Reason: string
}

/// <summary>
/// 仕訳明細項目レスポンス DTO（イベントソーシング用）
/// </summary>
[<CLIMutable>]
type JournalEntryLineItemResponse = {
    AccountCode: string
    DebitCredit: string
    Amount: decimal
}

module JournalEntryLineItemResponse =
    /// Domain Model からの変換
    let from (item: JournalEntryLineItem) : JournalEntryLineItemResponse =
        {
            AccountCode = item.AccountCode
            DebitCredit = item.DebitCredit.ToCode()
            Amount = item.Amount
        }

/// <summary>
/// 仕訳レスポンス DTO（イベントソーシング用）
/// </summary>
[<CLIMutable>]
type JournalEntryResponse = {
    Id: string
    EntryDate: DateTime
    Description: string
    Status: string
    Deleted: bool
    Version: int
    LineItems: JournalEntryLineItemResponse list
    TotalDebit: decimal
    TotalCredit: decimal
}

module JournalEntryResponse =
    /// Domain Model からの変換
    let from (aggregate: JournalEntryAggregate) : JournalEntryResponse =
        let statusStr =
            match aggregate.Status with
            | JournalEntryStatus.Draft -> "Draft"
            | JournalEntryStatus.Approved -> "Approved"

        let lineItems =
            aggregate.LineItems
            |> List.map JournalEntryLineItemResponse.from

        {
            Id = aggregate.Id
            EntryDate = aggregate.EntryDate
            Description = aggregate.Description
            Status = statusStr
            Deleted = aggregate.Deleted
            Version = aggregate.Version
            LineItems = lineItems
            TotalDebit = JournalEntryAggregate.sumDebit aggregate
            TotalCredit = JournalEntryAggregate.sumCredit aggregate
        }

    /// Domain Model リストからの変換
    let fromList (aggregates: JournalEntryAggregate list) : JournalEntryResponse list =
        aggregates |> List.map from
