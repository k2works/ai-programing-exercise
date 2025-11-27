namespace ManagementAccounting.Domain.Entities;

/// <summary>
/// 財務分析結果のキャッシュ
/// </summary>
public class FinancialAnalysisCache
{
    public int? Id { get; set; }
    public int FiscalYear { get; set; }
    public decimal Sales { get; set; }
    public decimal OperatingProfit { get; set; }
    public decimal OperatingProfitMargin { get; set; }
    public decimal TotalAssetTurnover { get; set; }
    public decimal EquityRatio { get; set; }
    public DateTime CalculatedAt { get; set; }

    public FinancialAnalysisCache()
    {
    }

    public FinancialAnalysisCache(int fiscalYear, FinancialData data, FinancialRatios ratios)
    {
        FiscalYear = fiscalYear;
        Sales = data.Sales;
        OperatingProfit = data.OperatingProfit;
        OperatingProfitMargin = ratios.OperatingProfitMargin;
        TotalAssetTurnover = ratios.TotalAssetTurnover;
        EquityRatio = ratios.EquityRatio;
        CalculatedAt = DateTime.UtcNow;
    }
}
