namespace AccountingSystem.Application.Port.Out

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Models

/// <summary>
/// 財務諸表リポジトリインターフェース
/// Infrastructure 層で実装される
/// </summary>
type IFinancialStatementRepository =
    /// <summary>
    /// 貸借対照表を生成する
    /// </summary>
    /// <param name="asOfDate">基準日</param>
    /// <returns>貸借対照表</returns>
    abstract member GenerateBalanceSheetAsync: asOfDate:DateTime -> Task<BalanceSheet>

    /// <summary>
    /// 損益計算書を生成する
    /// </summary>
    /// <param name="fromDate">開始日</param>
    /// <param name="toDate">終了日</param>
    /// <returns>損益計算書</returns>
    abstract member GenerateIncomeStatementAsync: fromDate:DateTime * toDate:DateTime -> Task<IncomeStatement>
