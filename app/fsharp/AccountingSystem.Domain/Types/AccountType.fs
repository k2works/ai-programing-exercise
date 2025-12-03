namespace AccountingSystem.Domain.Types

/// <summary>
/// 勘定科目種別
/// </summary>
type AccountType =
    | Asset       // 資産
    | Liability   // 負債
    | Equity      // 純資産
    | Revenue     // 収益
    | Expense     // 費用

    /// <summary>
    /// 文字列から AccountType に変換
    /// </summary>
    static member FromString(s: string) =
        match s with
        | "資産" -> Asset
        | "負債" -> Liability
        | "純資産" -> Equity
        | "収益" -> Revenue
        | "費用" -> Expense
        | _ -> failwith $"Unknown account type: {s}"

    /// <summary>
    /// AccountType を文字列に変換
    /// </summary>
    member this.ToDbString() =
        match this with
        | Asset -> "資産"
        | Liability -> "負債"
        | Equity -> "純資産"
        | Revenue -> "収益"
        | Expense -> "費用"
