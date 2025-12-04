namespace AccountingSystem.Application.Port.In

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Models

/// <summary>
/// 財務諸表ユースケースインターフェース
/// </summary>
type IFinancialStatementUseCase =
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

    /// <summary>
    /// 財務指標を計算する
    /// </summary>
    /// <param name="asOfDate">貸借対照表の基準日</param>
    /// <param name="fromDate">損益計算書の開始日</param>
    /// <param name="toDate">損益計算書の終了日</param>
    /// <returns>財務指標</returns>
    abstract member CalculateFinancialRatiosAsync: asOfDate:DateTime * fromDate:DateTime * toDate:DateTime -> Task<FinancialRatios>
