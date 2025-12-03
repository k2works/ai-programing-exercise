namespace AccountingSystem.Domain.Types

/// <summary>
/// 仕訳伝票区分
/// </summary>
type VoucherType =
    | Receipt    // 入金伝票 (1)
    | Payment    // 出金伝票 (2)
    | Transfer   // 振替伝票 (3)

module VoucherType =
    let toInt = function
        | Receipt -> 1
        | Payment -> 2
        | Transfer -> 3

    let fromInt = function
        | 1 -> Some Receipt
        | 2 -> Some Payment
        | 3 -> Some Transfer
        | _ -> None

    let toString = function
        | Receipt -> "入金"
        | Payment -> "出金"
        | Transfer -> "振替"
