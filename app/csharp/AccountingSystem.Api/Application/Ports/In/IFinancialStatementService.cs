using AccountingSystem.Domain.Models;

namespace AccountingSystem.Application.Ports.In;

/// <summary>
/// 財務諸表サービスインターフェース（入力ポート）
/// </summary>
public interface IFinancialStatementService
{
    /// <summary>
    /// 貸借対照表を生成
    /// </summary>
    Task<BalanceSheet> GenerateBalanceSheetAsync(DateOnly asOfDate);

    /// <summary>
    /// 損益計算書を生成
    /// </summary>
    Task<IncomeStatement> GenerateIncomeStatementAsync(DateOnly fromDate, DateOnly toDate);

    /// <summary>
    /// 財務指標を計算
    /// </summary>
    FinancialRatios CalculateFinancialRatios(BalanceSheet balanceSheet, IncomeStatement incomeStatement);
}
