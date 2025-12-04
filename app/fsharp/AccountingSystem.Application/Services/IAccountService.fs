namespace AccountingSystem.Application.Services

open System.Threading.Tasks
open AccountingSystem.Domain.Models

/// <summary>
/// 勘定科目サービスインターフェース
/// </summary>
type IAccountService =
    /// <summary>
    /// すべての勘定科目を取得
    /// </summary>
    abstract member GetAllAccountsAsync: unit -> Task<Account list>

    /// <summary>
    /// 科目コードで勘定科目を取得
    /// </summary>
    /// <param name="accountCode">科目コード</param>
    /// <returns>勘定科目（見つからない場合は AccountNotFoundException）</returns>
    abstract member GetAccountByCodeAsync: accountCode:string -> Task<Account>

    /// <summary>
    /// 勘定科目種別で勘定科目を取得
    /// </summary>
    /// <param name="accountType">勘定科目種別</param>
    /// <returns>勘定科目リスト</returns>
    abstract member GetAccountsByTypeAsync: accountType:string -> Task<Account list>

    /// <summary>
    /// 新しい勘定科目を作成
    /// </summary>
    /// <param name="account">勘定科目</param>
    /// <returns>作成された勘定科目</returns>
    abstract member CreateAccountAsync: account:Account -> Task<Account>

    /// <summary>
    /// 勘定科目を更新
    /// </summary>
    /// <param name="accountCode">科目コード</param>
    /// <param name="account">更新する勘定科目</param>
    /// <returns>更新された勘定科目</returns>
    abstract member UpdateAccountAsync: accountCode:string -> account:Account -> Task<Account>

    /// <summary>
    /// 勘定科目を削除
    /// </summary>
    /// <param name="accountCode">科目コード</param>
    abstract member DeleteAccountAsync: accountCode:string -> Task<unit>
