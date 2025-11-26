namespace AccountingSystem.Domain.Models;

/// <summary>
/// 財務諸表サービスのインターフェース
/// </summary>
public interface IFinancialStatementService
{
    /// <summary>
    /// 貸借対照表を生成する
    /// </summary>
    /// <param name="asOfDate">基準日</param>
    /// <returns>貸借対照表</returns>
    Task<BalanceSheet> GenerateBalanceSheetAsync(DateOnly asOfDate);

    /// <summary>
    /// 損益計算書を生成する
    /// </summary>
    /// <param name="fromDate">開始日</param>
    /// <param name="toDate">終了日</param>
    /// <returns>損益計算書</returns>
    Task<IncomeStatement> GenerateIncomeStatementAsync(DateOnly fromDate, DateOnly toDate);

    /// <summary>
    /// 財務指標を計算する
    /// </summary>
    /// <param name="balanceSheet">貸借対照表</param>
    /// <param name="incomeStatement">損益計算書</param>
    /// <returns>財務指標</returns>
    FinancialRatios CalculateFinancialRatios(BalanceSheet balanceSheet, IncomeStatement incomeStatement);
}
