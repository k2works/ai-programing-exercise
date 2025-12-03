namespace AccountingSystem.Domain

open System

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

/// <summary>
/// 勘定科目エンティティ
/// </summary>
type Account = {
    AccountId: int
    AccountCode: string
    AccountName: string
    AccountType: AccountType
    Balance: decimal
    BsplType: BsplType option
    TransactionElementType: TransactionElementType option
    ExpenseType: string option
    IsSummaryAccount: bool
    DisplayOrder: int option
    IsAggregationTarget: bool
    AccountKana: string option
    TaxTransactionCode: string option
    CreatedAt: DateTime
    UpdatedAt: DateTime
}

/// <summary>
/// 課税取引エンティティ
/// </summary>
type TaxTransaction = {
    TaxTransactionCode: string
    TaxTransactionName: string
    TaxRate: decimal
    EffectiveStartDate: DateTime
    EffectiveEndDate: DateTime option
    Description: string option
    CreatedAt: DateTime
    UpdatedAt: DateTime
}

/// <summary>
/// 勘定科目構成エンティティ
/// </summary>
type AccountStructure = {
    AccountCode: string
    AccountPath: string
    HierarchyLevel: int
    ParentAccountCode: string option
    DisplayOrder: int
    CreatedAt: DateTime
    UpdatedAt: DateTime
}

module AccountStructure =
    /// <summary>
    /// パスから階層レベルを計算
    /// </summary>
    let calculateLevel (path: string) =
        path.Split('~').Length

    /// <summary>
    /// パスから親科目コードを取得
    /// </summary>
    let getParentCode (path: string) =
        let segments = path.Split('~')
        if segments.Length > 1 then
            Some segments.[segments.Length - 2]
        else
            None

    /// <summary>
    /// 子孫科目かどうかを判定
    /// </summary>
    let isDescendantOf (ancestorCode: string) (structure: AccountStructure) =
        structure.AccountPath.Contains($"~{ancestorCode}~") ||
        structure.AccountCode = ancestorCode
