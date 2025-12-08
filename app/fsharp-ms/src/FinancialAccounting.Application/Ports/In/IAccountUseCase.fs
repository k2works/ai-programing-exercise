namespace FinancialAccounting.Application.Ports.In

open System.Threading.Tasks
open FinancialAccounting.Domain.Models

/// <summary>
/// 勘定科目作成リクエスト
/// </summary>
type CreateAccountRequest = {
    AccountCode: string
    AccountName: string
    AccountNameKana: string option
    AccountType: string
    IsSummaryAccount: bool
    BsPlType: string
    TransactionElementType: string
    ExpenseType: string option
    DisplayOrder: int
    IsAggregationTarget: bool
    TaxCode: string option
}

/// <summary>
/// 勘定科目更新リクエスト
/// </summary>
type UpdateAccountRequest = {
    AccountId: int
    AccountName: string
    AccountNameKana: string option
    IsSummaryAccount: bool
    ExpenseType: string option
    DisplayOrder: int
    IsAggregationTarget: bool
    TaxCode: string option
}

/// <summary>
/// 勘定科目ユースケースインターフェース（入力ポート）
/// </summary>
type IAccountUseCase =
    /// <summary>
    /// 勘定科目を作成
    /// </summary>
    abstract member CreateAccountAsync: CreateAccountRequest -> Task<Result<Account, string>>

    /// <summary>
    /// IDで勘定科目を取得
    /// </summary>
    abstract member GetAccountByIdAsync: int -> Task<Account option>

    /// <summary>
    /// 勘定科目コードで勘定科目を取得
    /// </summary>
    abstract member GetAccountByCodeAsync: string -> Task<Account option>

    /// <summary>
    /// 全ての勘定科目を取得
    /// </summary>
    abstract member GetAllAccountsAsync: unit -> Task<Account list>

    /// <summary>
    /// 勘定科目種別で勘定科目を取得
    /// </summary>
    abstract member GetAccountsByTypeAsync: string -> Task<Result<Account list, string>>

    /// <summary>
    /// 勘定科目を更新
    /// </summary>
    abstract member UpdateAccountAsync: UpdateAccountRequest -> Task<Result<Account, string>>

    /// <summary>
    /// 勘定科目を削除
    /// </summary>
    abstract member DeleteAccountAsync: int -> Task<Result<bool, string>>
