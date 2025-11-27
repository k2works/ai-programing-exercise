using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Entities;
using AccountingSystem.Domain.Models.Financial;
using Microsoft.Extensions.Logging;

namespace AccountingSystem.Application.Services;

/// <summary>
/// 財務分析サービス実装
/// D社の月次勘定科目残高データから財務指標を計算する
/// </summary>
public class FinancialAnalysisService : IFinancialAnalysisService
{
    private readonly IMonthlyAccountBalanceRepository _monthlyBalanceRepository;
    private readonly FinancialRatioAnalyzer _analyzer;
    private readonly ILogger<FinancialAnalysisService> _logger;

    // 会計年度の期末月（3月決算の場合は12月目）
    private const int FiscalYearEndMonth = 12;

    public FinancialAnalysisService(
        IMonthlyAccountBalanceRepository monthlyBalanceRepository,
        ILogger<FinancialAnalysisService> logger)
    {
        _monthlyBalanceRepository = monthlyBalanceRepository;
        _analyzer = new FinancialRatioAnalyzer();
        _logger = logger;
    }

    /// <inheritdoc />
    public async Task<AnalysisResult> AnalyzeAsync(int fiscalYear)
    {
        _logger.LogInformation("財務分析を開始: 決算期={FiscalYear}", fiscalYear);

        // 期末月（12月目）の月次残高を取得
        var monthlyBalances = await _monthlyBalanceRepository
            .FindByFiscalYearAndMonthAsync(fiscalYear, FiscalYearEndMonth);

        if (monthlyBalances.Count == 0)
        {
            throw new InvalidOperationException(
                $"決算期 {fiscalYear} のデータが見つかりません。");
        }

        // MonthlyAccountBalance を MonthlyBalanceData に変換
        var balanceData = monthlyBalances
            .Select(b => new MonthlyBalanceData(
                b.AccountCode,
                b.DebitAmount,
                b.CreditAmount))
            .ToList();

        // FinancialData を生成
        var financialData = FinancialData.FromMonthlyBalances(fiscalYear, balanceData);

        // 財務分析を実行
        var result = _analyzer.Analyze(financialData);

        _logger.LogInformation(
            "財務分析が完了: 決算期={FiscalYear}, 営業利益率={OperatingProfitMargin}%",
            fiscalYear,
            result.Profitability.OperatingProfitMargin);

        return result;
    }

    /// <inheritdoc />
    public async Task<IReadOnlyList<AnalysisResult>> AnalyzeRangeAsync(int fromFiscalYear, int toFiscalYear)
    {
        if (fromFiscalYear > toFiscalYear)
        {
            throw new ArgumentException(
                $"開始年度 ({fromFiscalYear}) は終了年度 ({toFiscalYear}) 以前である必要があります。");
        }

        _logger.LogInformation(
            "期間財務分析を開始: {FromYear}～{ToYear}",
            fromFiscalYear,
            toFiscalYear);

        var results = new List<AnalysisResult>();

        for (var year = fromFiscalYear; year <= toFiscalYear; year++)
        {
            try
            {
                var result = await AnalyzeAsync(year);
                results.Add(result);
            }
            catch (InvalidOperationException ex)
            {
                _logger.LogWarning(
                    ex,
                    "決算期 {FiscalYear} のデータが見つかりません。スキップします。",
                    year);
            }
        }

        _logger.LogInformation(
            "期間財務分析が完了: {Count} 期間のデータを分析",
            results.Count);

        return results;
    }
}
