namespace AccountingSystem.Application.Repositories

open System.Threading.Tasks
open AccountingSystem.Domain.Models

/// <summary>
/// 勘定科目リポジトリインターフェース（Output Port）
/// Infrastructure 層で実装される
/// </summary>
type IAccountRepository =
    /// <summary>
    /// すべての勘定科目を取得
    /// </summary>
    /// <returns>勘定科目リスト</returns>
    abstract member FindAllAsync: unit -> Task<Account list>

    /// <summary>
    /// 科目コードで勘定科目を検索
    /// </summary>
    /// <param name="accountCode">科目コード</param>
    /// <returns>勘定科目（存在しない場合は None）</returns>
    abstract member FindByCodeAsync: accountCode:string -> Task<Account option>

    /// <summary>
    /// 勘定科目IDで勘定科目を検索
    /// </summary>
    /// <param name="accountId">勘定科目ID</param>
    /// <returns>勘定科目（存在しない場合は None）</returns>
    abstract member FindByIdAsync: accountId:int -> Task<Account option>

    /// <summary>
    /// 勘定科目種別で勘定科目を検索
    /// </summary>
    /// <param name="accountType">勘定科目種別</param>
    /// <returns>勘定科目リスト</returns>
    abstract member FindByTypeAsync: accountType:string -> Task<Account list>

    /// <summary>
    /// 勘定科目を保存（作成）
    /// </summary>
    /// <param name="account">勘定科目</param>
    /// <returns>保存された勘定科目のID</returns>
    abstract member SaveAsync: account:Account -> Task<int>

    /// <summary>
    /// 勘定科目を更新
    /// </summary>
    /// <param name="account">勘定科目</param>
    /// <returns>更新された行数</returns>
    abstract member UpdateAsync: account:Account -> Task<int>

    /// <summary>
    /// 勘定科目を削除
    /// </summary>
    /// <param name="accountCode">科目コード</param>
    /// <returns>削除された行数</returns>
    abstract member DeleteByCodeAsync: accountCode:string -> Task<int>
