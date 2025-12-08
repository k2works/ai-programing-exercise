namespace FinancialAccounting.Application.Ports.Out

open System.Threading.Tasks
open FinancialAccounting.Domain.Models

/// <summary>
/// 仕訳リポジトリのポート（出力ポート）
/// </summary>
type IJournalRepository =
    /// <summary>
    /// 仕訳を保存
    /// </summary>
    abstract member SaveAsync: Journal -> Task<Journal>

    /// <summary>
    /// IDで仕訳を取得
    /// </summary>
    abstract member GetByIdAsync: int -> Task<Journal option>

    /// <summary>
    /// 会計年度で仕訳一覧を取得
    /// </summary>
    abstract member GetByFiscalYearAsync: int -> Task<Journal list>
