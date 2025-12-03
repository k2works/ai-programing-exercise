namespace AccountingSystem.Domain.Types

/// <summary>
/// 取引要素区分
/// </summary>
type TransactionElementType =
    | AssetElement      // 資産
    | LiabilityElement  // 負債
    | EquityElement     // 純資産
    | RevenueElement    // 収益
    | ExpenseElement    // 費用

    member this.ToCode() =
        match this with
        | AssetElement -> "1"
        | LiabilityElement -> "2"
        | EquityElement -> "3"
        | RevenueElement -> "4"
        | ExpenseElement -> "5"

    static member FromCode(code: string) =
        match code with
        | "1" -> Some AssetElement
        | "2" -> Some LiabilityElement
        | "3" -> Some EquityElement
        | "4" -> Some RevenueElement
        | "5" -> Some ExpenseElement
        | null | "" -> None
        | _ -> failwith $"Unknown transaction element code: {code}"
