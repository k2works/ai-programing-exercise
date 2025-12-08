namespace FinancialAccounting.Application.Ports.Out

open System.Threading.Tasks
open FinancialAccounting.Domain.Models

/// <summary>
/// 勘定科目リポジトリインターフェース（出力ポート）
/// </summary>
type IAccountRepository =
    /// <summary>
    /// 勘定科目を保存
    /// </summary>
    abstract member SaveAsync: Account -> Task<Account>

    /// <summary>
    /// IDで勘定科目を取得
    /// </summary>
    abstract member GetByIdAsync: int -> Task<Account option>

    /// <summary>
    /// 勘定科目コードで勘定科目を取得
    /// </summary>
    abstract member GetByCodeAsync: string -> Task<Account option>

    /// <summary>
    /// 全ての勘定科目を取得
    /// </summary>
    abstract member GetAllAsync: unit -> Task<Account list>

    /// <summary>
    /// 勘定科目種別で勘定科目を取得
    /// </summary>
    abstract member GetByTypeAsync: AccountType -> Task<Account list>

    /// <summary>
    /// 勘定科目を更新
    /// </summary>
    abstract member UpdateAsync: Account -> Task<Account>

    /// <summary>
    /// 勘定科目を削除
    /// </summary>
    abstract member DeleteAsync: int -> Task<bool>
