namespace AccountingSystem.Api.Dtos

open System.ComponentModel.DataAnnotations
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Models

/// <summary>
/// 勘定科目リクエスト DTO
/// </summary>
[<CLIMutable>]
type AccountRequest = {
    [<Required(ErrorMessage = "勘定科目コードは必須です")>]
    [<MaxLength(10, ErrorMessage = "勘定科目コードは10文字以内である必要があります")>]
    AccountCode: string

    [<Required(ErrorMessage = "勘定科目名は必須です")>]
    [<MaxLength(40, ErrorMessage = "勘定科目名は40文字以内である必要があります")>]
    AccountName: string

    [<MaxLength(40, ErrorMessage = "勘定科目カナは40文字以内である必要があります")>]
    AccountNameKana: string

    [<Required(ErrorMessage = "勘定科目種別は必須です")>]
    AccountType: string

    IsSummaryAccount: bool

    BsplType: string

    TransactionElementType: string

    ExpenseType: string

    DisplayOrder: int

    IsAggregationTarget: bool

    TaxCode: string
}

module AccountRequest =
    /// <summary>
    /// Domain Model への変換
    /// </summary>
    let toDomain (request: AccountRequest) : Account =
        let accountType =
            match request.AccountType with
            | "資産" -> AccountType.Asset
            | "負債" -> AccountType.Liability
            | "純資産" -> AccountType.Equity
            | "収益" -> AccountType.Revenue
            | "費用" -> AccountType.Expense
            | _ -> AccountType.Asset

        let bsplType =
            match request.BsplType with
            | null | "" -> None
            | "B" -> Some BsplType.BalanceSheet
            | "P" -> Some BsplType.ProfitAndLoss
            | _ -> None

        let transactionElementType =
            match request.TransactionElementType with
            | null | "" -> None
            | "1" -> Some TransactionElementType.AssetElement
            | "2" -> Some TransactionElementType.LiabilityElement
            | "3" -> Some TransactionElementType.EquityElement
            | "4" -> Some TransactionElementType.RevenueElement
            | "5" -> Some TransactionElementType.ExpenseElement
            | _ -> None

        {
            AccountId = None
            AccountCode = AccountCode.Create(request.AccountCode)
            AccountName = request.AccountName
            AccountNameKana = if System.String.IsNullOrEmpty(request.AccountNameKana) then None else Some request.AccountNameKana
            AccountType = accountType
            IsSummaryAccount = request.IsSummaryAccount
            BsplType = bsplType
            TransactionElementType = transactionElementType
            ExpenseType = if System.String.IsNullOrEmpty(request.ExpenseType) then None else Some request.ExpenseType
            DisplayOrder = request.DisplayOrder
            IsAggregationTarget = request.IsAggregationTarget
            TaxCode = if System.String.IsNullOrEmpty(request.TaxCode) then None else Some request.TaxCode
            Balance = Money.Zero
        }

/// <summary>
/// 勘定科目レスポンス DTO
/// </summary>
[<CLIMutable>]
type AccountResponse = {
    AccountCode: string
    AccountName: string
    AccountNameKana: string option
    AccountType: string
    IsSummaryAccount: bool
    BsplType: string option
    TransactionElementType: string option
    ExpenseType: string option
    DisplayOrder: int
    IsAggregationTarget: bool
    TaxCode: string option
    Balance: decimal
}

module AccountResponse =
    /// <summary>
    /// Domain Model からの変換
    /// </summary>
    let from (account: Account) : AccountResponse =
        let accountTypeStr =
            match account.AccountType with
            | AccountType.Asset -> "資産"
            | AccountType.Liability -> "負債"
            | AccountType.Equity -> "純資産"
            | AccountType.Revenue -> "収益"
            | AccountType.Expense -> "費用"

        let bsplTypeStr =
            account.BsplType
            |> Option.map (fun b ->
                match b with
                | BsplType.BalanceSheet -> "B"
                | BsplType.ProfitAndLoss -> "P")

        let transactionElementTypeStr =
            account.TransactionElementType
            |> Option.map (fun t ->
                match t with
                | TransactionElementType.AssetElement -> "1"
                | TransactionElementType.LiabilityElement -> "2"
                | TransactionElementType.EquityElement -> "3"
                | TransactionElementType.RevenueElement -> "4"
                | TransactionElementType.ExpenseElement -> "5")

        {
            AccountCode = account.AccountCode.Code
            AccountName = account.AccountName
            AccountNameKana = account.AccountNameKana
            AccountType = accountTypeStr
            IsSummaryAccount = account.IsSummaryAccount
            BsplType = bsplTypeStr
            TransactionElementType = transactionElementTypeStr
            ExpenseType = account.ExpenseType
            DisplayOrder = account.DisplayOrder
            IsAggregationTarget = account.IsAggregationTarget
            TaxCode = account.TaxCode
            Balance = Money.toDecimal account.Balance
        }

    /// <summary>
    /// Domain Model リストからの変換
    /// </summary>
    let fromList (accounts: Account list) : AccountResponse list =
        accounts |> List.map from
