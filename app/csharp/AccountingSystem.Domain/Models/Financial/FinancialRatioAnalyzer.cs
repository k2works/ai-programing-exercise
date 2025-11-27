namespace AccountingSystem.Domain.Models.Financial;

/// <summary>
/// 財務分析指標を計算するクラス
/// D社の事例に基づき、収益性・効率性・安全性の各指標を計算します。
/// </summary>
public class FinancialRatioAnalyzer
{
    private const int Scale = 2; // 小数点以下の桁数

    /// <summary>
    /// 財務データを分析して各種指標を計算
    /// </summary>
    public AnalysisResult Analyze(FinancialData data)
    {
        ValidateData(data);

        return new AnalysisResult(
            data.FiscalYear,
            CalculateProfitability(data),
            CalculateEfficiency(data),
            CalculateSafety(data)
        );
    }

    private static void ValidateData(FinancialData data)
    {
        if (data.Sales == 0)
        {
            throw new ArgumentException("売上高がゼロのため計算できません");
        }

        if (data.TotalAssets == 0)
        {
            throw new ArgumentException("総資産がゼロのため計算できません");
        }

        if (data.CurrentLiabilities == 0)
        {
            throw new ArgumentException("流動負債がゼロのため計算できません");
        }
    }

    private static Profitability CalculateProfitability(FinancialData data)
    {
        return new Profitability(
            GrossProfitMargin: CalculateRatio(data.GrossProfit, data.Sales),
            OperatingProfitMargin: CalculateRatio(data.OperatingProfit, data.Sales),
            OrdinaryProfitMargin: CalculateRatio(data.OrdinaryProfit, data.Sales),
            SellingExpenseRatio: CalculateRatio(data.SellingExpenses, data.Sales)
        );
    }

    private static Efficiency CalculateEfficiency(FinancialData data)
    {
        return new Efficiency(
            TotalAssetTurnover: CalculateTurnover(data.Sales, data.TotalAssets),
            ReceivablesTurnover: data.ReceivablesTurnover > 0
                ? Round(data.ReceivablesTurnover)
                : 0m,
            InventoryTurnover: data.Inventory > 0
                ? CalculateTurnover(data.Sales, data.Inventory)
                : 0m,
            TangibleFixedAssetTurnover: data.TangibleFixedAssets > 0
                ? CalculateTurnover(data.Sales, data.TangibleFixedAssets)
                : 0m
        );
    }

    private static Safety CalculateSafety(FinancialData data)
    {
        var fixedAssets = data.FixedAssets;

        return new Safety(
            CurrentRatio: CalculateRatio(data.CurrentAssets, data.CurrentLiabilities),
            QuickRatio: CalculateRatio(data.QuickAssets, data.CurrentLiabilities),
            FixedRatio: data.Equity > 0
                ? CalculateRatio(fixedAssets, data.Equity)
                : 0m,
            FixedLongTermRatio: (data.Equity + data.FixedLiabilities) > 0
                ? CalculateRatio(fixedAssets, data.Equity + data.FixedLiabilities)
                : 0m,
            DebtRatio: data.Equity > 0
                ? CalculateRatio(data.TotalLiabilities, data.Equity)
                : 0m,
            EquityRatio: CalculateRatio(data.Equity, data.TotalAssets)
        );
    }

    /// <summary>
    /// 比率を計算（パーセンテージ）
    /// </summary>
    private static decimal CalculateRatio(decimal numerator, decimal denominator)
    {
        if (denominator == 0)
        {
            return 0m;
        }

        return Round(numerator / denominator * 100);
    }

    /// <summary>
    /// 回転率を計算
    /// </summary>
    private static decimal CalculateTurnover(decimal numerator, decimal denominator)
    {
        if (denominator == 0)
        {
            return 0m;
        }

        return Round(numerator / denominator);
    }

    private static decimal Round(decimal value)
    {
        return Math.Round(value, Scale, MidpointRounding.AwayFromZero);
    }
}
