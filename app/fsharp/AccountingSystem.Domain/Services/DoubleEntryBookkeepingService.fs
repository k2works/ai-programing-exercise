module AccountingSystem.Domain.Services.DoubleEntryBookkeepingService

open System

/// <summary>
/// 不整合仕訳の情報
/// </summary>
type InconsistentJournal = {
    /// 伝票番号
    VoucherNumber: string
    /// 借方合計
    DebitTotal: decimal
    /// 貸方合計
    CreditTotal: decimal
    /// 差額
    Difference: decimal
}

/// <summary>
/// 複式簿記チェックの結果
/// </summary>
type DoubleEntryCheckResult =
    | Valid
    | Invalid of InconsistentJournal list

module DoubleEntryCheckResult =
    /// 結果が有効かどうか
    let isValid = function
        | Valid -> true
        | Invalid _ -> false

    /// 不整合リストを取得（有効な場合は空リスト）
    let getInconsistencies = function
        | Valid -> []
        | Invalid journals -> journals

    /// 結果をメッセージに変換
    let toMessage = function
        | Valid -> "すべての仕訳が複式簿記の原理を満たしています"
        | Invalid journals ->
            let count = List.length journals
            sprintf "%d 件の仕訳に不整合があります" count
