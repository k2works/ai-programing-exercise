namespace AccountingSystem.Domain.Types

/// <summary>
/// 決算仕訳フラグ
/// </summary>
type SettlementFlag =
    | Normal     // 通常仕訳 (0)
    | Settlement // 決算仕訳 (1)

module SettlementFlag =
    let toInt = function
        | Normal -> 0
        | Settlement -> 1

    let fromInt = function
        | 0 -> Some Normal
        | 1 -> Some Settlement
        | _ -> None

    let toString = function
        | Normal -> "通常"
        | Settlement -> "決算"
