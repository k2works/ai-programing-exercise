namespace AccountingSystem.Domain.Types

/// <summary>
/// BSPL区分
/// </summary>
type BsplType =
    | BalanceSheet   // 貸借対照表
    | ProfitAndLoss  // 損益計算書

    member this.ToCode() =
        match this with
        | BalanceSheet -> "B"
        | ProfitAndLoss -> "P"

    static member FromCode(code: string) =
        match code with
        | "B" -> Some BalanceSheet
        | "P" -> Some ProfitAndLoss
        | null | "" -> None
        | _ -> failwith $"Unknown BSPL code: {code}"
