namespace AccountingSystem.Domain.Types

/// <summary>
/// 貸借区分
/// </summary>
type DebitCreditType =
    | Debit   // 借方
    | Credit  // 貸方

    member this.ToCode() =
        match this with
        | Debit -> "D"
        | Credit -> "C"

    static member FromCode(code: string) =
        match code with
        | "D" -> Some Debit
        | "C" -> Some Credit
        | null | "" -> None
        | _ -> failwith $"Unknown debit/credit code: {code}"
