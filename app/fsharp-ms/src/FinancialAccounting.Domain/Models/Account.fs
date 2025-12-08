namespace FinancialAccounting.Domain.Models

open System

/// <summary>
/// 勘定科目種別
/// </summary>
type AccountType =
    | Asset         // 資産
    | Liability     // 負債
    | Equity        // 純資産
    | Revenue       // 収益
    | Expense       // 費用

/// <summary>
/// BS/PL区分
/// </summary>
type BsPlType =
    | BalanceSheet      // 貸借対照表
    | ProfitAndLoss     // 損益計算書

/// <summary>
/// 取引要素区分
/// </summary>
type TransactionElementType =
    | Debit     // 借方
    | Credit    // 貸方

/// <summary>
/// 勘定科目エンティティ
/// </summary>
type Account = {
    AccountId: int option
    AccountCode: string
    AccountName: string
    AccountNameKana: string option
    AccountType: AccountType
    IsSummaryAccount: bool
    BsPlType: BsPlType
    TransactionElementType: TransactionElementType
    ExpenseType: string option
    DisplayOrder: int
    IsAggregationTarget: bool
    TaxCode: string option
    Balance: decimal
    CreatedAt: DateTime
    UpdatedAt: DateTime
}

module Account =
    /// <summary>
    /// AccountType を文字列に変換
    /// </summary>
    let accountTypeToString (accountType: AccountType) : string =
        match accountType with
        | Asset -> "Asset"
        | Liability -> "Liability"
        | Equity -> "Equity"
        | Revenue -> "Revenue"
        | Expense -> "Expense"

    /// <summary>
    /// 文字列を AccountType に変換
    /// </summary>
    let stringToAccountType (s: string) : AccountType option =
        match s.ToLowerInvariant() with
        | "asset" -> Some Asset
        | "liability" -> Some Liability
        | "equity" -> Some Equity
        | "revenue" -> Some Revenue
        | "expense" -> Some Expense
        | _ -> None

    /// <summary>
    /// BsPlType を文字列に変換
    /// </summary>
    let bsPlTypeToString (bsPlType: BsPlType) : string =
        match bsPlType with
        | BalanceSheet -> "B"
        | ProfitAndLoss -> "P"

    /// <summary>
    /// 文字列を BsPlType に変換
    /// </summary>
    let stringToBsPlType (s: string) : BsPlType option =
        match s.ToUpperInvariant() with
        | "B" -> Some BalanceSheet
        | "P" -> Some ProfitAndLoss
        | _ -> None

    /// <summary>
    /// TransactionElementType を文字列に変換
    /// </summary>
    let transactionElementTypeToString (elementType: TransactionElementType) : string =
        match elementType with
        | Debit -> "Debit"
        | Credit -> "Credit"

    /// <summary>
    /// 文字列を TransactionElementType に変換
    /// </summary>
    let stringToTransactionElementType (s: string) : TransactionElementType option =
        match s.ToLowerInvariant() with
        | "debit" -> Some Debit
        | "credit" -> Some Credit
        | _ -> None

    /// <summary>
    /// 新しい勘定科目を作成
    /// </summary>
    let create
        (accountCode: string)
        (accountName: string)
        (accountType: AccountType)
        (bsPlType: BsPlType)
        (transactionElementType: TransactionElementType)
        : Result<Account, string> =

        if String.IsNullOrWhiteSpace(accountCode) then
            Error "勘定科目コードは必須です"
        elif String.IsNullOrWhiteSpace(accountName) then
            Error "勘定科目名は必須です"
        else
            Ok {
                AccountId = None
                AccountCode = accountCode
                AccountName = accountName
                AccountNameKana = None
                AccountType = accountType
                IsSummaryAccount = false
                BsPlType = bsPlType
                TransactionElementType = transactionElementType
                ExpenseType = None
                DisplayOrder = 0
                IsAggregationTarget = true
                TaxCode = None
                Balance = 0m
                CreatedAt = DateTime.UtcNow
                UpdatedAt = DateTime.UtcNow
            }

    /// <summary>
    /// 勘定科目コードのバリデーション
    /// </summary>
    let validateAccountCode (accountCode: string) : Result<string, string> =
        if String.IsNullOrWhiteSpace(accountCode) then
            Error "勘定科目コードは必須です"
        elif accountCode.Length > 20 then
            Error "勘定科目コードは20文字以内にしてください"
        else
            Ok accountCode
