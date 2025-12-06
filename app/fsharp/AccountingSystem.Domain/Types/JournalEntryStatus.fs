namespace AccountingSystem.Domain.Types

/// <summary>
/// 仕訳ステータス（イベントソーシング用）
/// </summary>
type JournalEntryStatus =
    | Draft      // 下書き
    | Approved   // 承認済み

module JournalEntryStatus =
    let toCode = function
        | Draft -> "DRAFT"
        | Approved -> "APPROVED"

    let fromCode = function
        | "DRAFT" -> Some Draft
        | "APPROVED" -> Some Approved
        | _ -> None
