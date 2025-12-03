namespace AccountingSystem.Domain.Types

/// <summary>
/// 赤伝フラグ
/// </summary>
type RedSlipFlag =
    | Normal  // 通常伝票 (0)
    | RedSlip // 赤伝票 (1)

module RedSlipFlag =
    let toInt = function
        | Normal -> 0
        | RedSlip -> 1

    let fromInt = function
        | 0 -> Some Normal
        | 1 -> Some RedSlip
        | _ -> None

    let toString = function
        | Normal -> "通常"
        | RedSlip -> "赤伝"
