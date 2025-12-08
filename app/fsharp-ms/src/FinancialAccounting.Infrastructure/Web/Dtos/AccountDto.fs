namespace FinancialAccounting.Infrastructure.Web.Dtos

open System
open FinancialAccounting.Domain.Models
open FinancialAccounting.Application.Ports.In

/// <summary>
/// 勘定科目作成リクエスト DTO
/// </summary>
[<CLIMutable>]
type AccountRequestDto = {
    AccountCode: string
    AccountName: string
    AccountNameKana: string
    AccountType: string
    IsSummaryAccount: bool
    BsPlType: string
    TransactionElementType: string
    ExpenseType: string
    DisplayOrder: int
    IsAggregationTarget: bool
    TaxCode: string
}

/// <summary>
/// 勘定科目更新リクエスト DTO
/// </summary>
[<CLIMutable>]
type AccountUpdateRequestDto = {
    AccountName: string
    AccountNameKana: string
    IsSummaryAccount: bool
    ExpenseType: string
    DisplayOrder: int
    IsAggregationTarget: bool
    TaxCode: string
}

/// <summary>
/// 勘定科目レスポンス DTO
/// </summary>
[<CLIMutable>]
type AccountResponseDto = {
    AccountId: int
    AccountCode: string
    AccountName: string
    AccountNameKana: string
    AccountType: string
    IsSummaryAccount: bool
    BsPlType: string
    TransactionElementType: string
    ExpenseType: string
    DisplayOrder: int
    IsAggregationTarget: bool
    TaxCode: string
    Balance: decimal
    CreatedAt: DateTime
    UpdatedAt: DateTime
}

module AccountDto =
    /// <summary>
    /// リクエスト DTO を UseCase リクエストに変換
    /// </summary>
    let toCreateRequest (dto: AccountRequestDto) : CreateAccountRequest =
        {
            AccountCode = dto.AccountCode
            AccountName = dto.AccountName
            AccountNameKana = if String.IsNullOrEmpty(dto.AccountNameKana) then None else Some dto.AccountNameKana
            AccountType = dto.AccountType
            IsSummaryAccount = dto.IsSummaryAccount
            BsPlType = dto.BsPlType
            TransactionElementType = dto.TransactionElementType
            ExpenseType = if String.IsNullOrEmpty(dto.ExpenseType) then None else Some dto.ExpenseType
            DisplayOrder = dto.DisplayOrder
            IsAggregationTarget = dto.IsAggregationTarget
            TaxCode = if String.IsNullOrEmpty(dto.TaxCode) then None else Some dto.TaxCode
        }

    /// <summary>
    /// 更新リクエスト DTO を UseCase リクエストに変換
    /// </summary>
    let toUpdateRequest (id: int) (dto: AccountUpdateRequestDto) : UpdateAccountRequest =
        {
            AccountId = id
            AccountName = dto.AccountName
            AccountNameKana = if String.IsNullOrEmpty(dto.AccountNameKana) then None else Some dto.AccountNameKana
            IsSummaryAccount = dto.IsSummaryAccount
            ExpenseType = if String.IsNullOrEmpty(dto.ExpenseType) then None else Some dto.ExpenseType
            DisplayOrder = dto.DisplayOrder
            IsAggregationTarget = dto.IsAggregationTarget
            TaxCode = if String.IsNullOrEmpty(dto.TaxCode) then None else Some dto.TaxCode
        }

    /// <summary>
    /// ドメインエンティティをレスポンス DTO に変換
    /// </summary>
    let toResponse (account: Account) : AccountResponseDto =
        {
            AccountId = account.AccountId |> Option.defaultValue 0
            AccountCode = account.AccountCode
            AccountName = account.AccountName
            AccountNameKana = account.AccountNameKana |> Option.defaultValue ""
            AccountType = Account.accountTypeToString account.AccountType
            IsSummaryAccount = account.IsSummaryAccount
            BsPlType = Account.bsPlTypeToString account.BsPlType
            TransactionElementType = Account.transactionElementTypeToString account.TransactionElementType
            ExpenseType = account.ExpenseType |> Option.defaultValue ""
            DisplayOrder = account.DisplayOrder
            IsAggregationTarget = account.IsAggregationTarget
            TaxCode = account.TaxCode |> Option.defaultValue ""
            Balance = account.Balance
            CreatedAt = account.CreatedAt
            UpdatedAt = account.UpdatedAt
        }
