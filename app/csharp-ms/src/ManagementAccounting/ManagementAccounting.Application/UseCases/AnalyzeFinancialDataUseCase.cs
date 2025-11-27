using ManagementAccounting.Application.Ports.In;
using ManagementAccounting.Application.Ports.Out;
using ManagementAccounting.Domain.Entities;

namespace ManagementAccounting.Application.UseCases;

/// <summary>
/// 財務分析ユースケース実装
/// </summary>
public class AnalyzeFinancialDataUseCase : IAnalyzeFinancialDataUseCase
{
    private readonly IFinancialAccountingClient _client;
    private readonly IFinancialAnalysisCacheRepository _cacheRepository;

    public AnalyzeFinancialDataUseCase(
        IFinancialAccountingClient client,
        IFinancialAnalysisCacheRepository cacheRepository)
    {
        _client = client;
        _cacheRepository = cacheRepository;
    }

    public async Task<FinancialAnalysisResult> AnalyzeAsync(int fiscalYear)
    {
        // 1. 財務会計サービスからデータ取得（腐敗防止層経由）
        var data = await _client.FetchFinancialDataByFiscalYearAsync(fiscalYear);

        // 2. 財務比率の計算
        var ratios = CalculateRatios(data);

        // 3. 結果をキャッシュ
        var cache = new FinancialAnalysisCache(fiscalYear, data, ratios);
        await _cacheRepository.SaveAsync(cache);

        return new FinancialAnalysisResult(data, ratios);
    }

    public async Task<List<FinancialAnalysisResult>> CompareAsync(List<int> fiscalYears)
    {
        var results = new List<FinancialAnalysisResult>();

        foreach (var fiscalYear in fiscalYears.OrderBy(y => y))
        {
            var result = await AnalyzeAsync(fiscalYear);
            results.Add(result);
        }

        return results;
    }

    /// <summary>
    /// 財務比率を計算
    /// </summary>
    private FinancialRatios CalculateRatios(FinancialData data)
    {
        // 営業利益率 = 営業利益 / 売上高
        var operatingProfitMargin = data.Sales != 0
            ? Math.Round(data.OperatingProfit / data.Sales * 100, 2)
            : 0;

        // 総資産回転率 = 売上高 / 総資産
        var totalAssetTurnover = data.TotalAssets != 0
            ? Math.Round(data.Sales / data.TotalAssets, 2)
            : 0;

        // 固定資産回転率 = 売上高 / 有形固定資産
        var fixedAssetTurnover = data.TangibleFixedAssets != 0
            ? Math.Round(data.Sales / data.TangibleFixedAssets, 2)
            : 0;

        // 流動比率 = 流動資産 / 流動負債 * 100
        var currentRatio = data.CurrentLiabilities != 0
            ? Math.Round(data.CurrentAssets / data.CurrentLiabilities * 100, 2)
            : 0;

        // 当座比率 = 当座資産 / 流動負債 * 100
        var quickRatio = data.CurrentLiabilities != 0
            ? Math.Round(data.QuickAssets / data.CurrentLiabilities * 100, 2)
            : 0;

        // 自己資本比率 = 自己資本 / 総資産 * 100
        var equityRatio = data.TotalAssets != 0
            ? Math.Round(data.Equity / data.TotalAssets * 100, 2)
            : 0;

        return new FinancialRatios(
            operatingProfitMargin,
            totalAssetTurnover,
            fixedAssetTurnover,
            currentRatio,
            quickRatio,
            equityRatio
        );
    }
}
