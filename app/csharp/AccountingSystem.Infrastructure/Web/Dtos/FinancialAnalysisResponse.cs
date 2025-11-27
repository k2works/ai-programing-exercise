using AccountingSystem.Domain.Models.Financial;

namespace AccountingSystem.Infrastructure.Web.Dtos;

/// <summary>
/// 財務分析レスポンス DTO
/// </summary>
public record FinancialAnalysisResponse
{
    /// <summary>
    /// 決算期
    /// </summary>
    public required int FiscalYear { get; init; }

    /// <summary>
    /// 収益性指標
    /// </summary>
    public required ProfitabilityResponse Profitability { get; init; }

    /// <summary>
    /// 効率性指標
    /// </summary>
    public required EfficiencyResponse Efficiency { get; init; }

    /// <summary>
    /// 安全性指標
    /// </summary>
    public required SafetyResponse Safety { get; init; }

    /// <summary>
    /// ドメインモデルからレスポンスを生成
    /// </summary>
    public static FinancialAnalysisResponse From(AnalysisResult result)
    {
        return new FinancialAnalysisResponse
        {
            FiscalYear = result.FiscalYear,
            Profitability = ProfitabilityResponse.From(result.Profitability),
            Efficiency = EfficiencyResponse.From(result.Efficiency),
            Safety = SafetyResponse.From(result.Safety)
        };
    }
}

/// <summary>
/// 収益性指標レスポンス
/// </summary>
public record ProfitabilityResponse
{
    /// <summary>
    /// 売上高総利益率（%）
    /// </summary>
    public required decimal GrossProfitMargin { get; init; }

    /// <summary>
    /// 売上高営業利益率（%）
    /// </summary>
    public required decimal OperatingProfitMargin { get; init; }

    /// <summary>
    /// 売上高経常利益率（%）
    /// </summary>
    public required decimal OrdinaryProfitMargin { get; init; }

    /// <summary>
    /// 売上高販管費比率（%）
    /// </summary>
    public required decimal SellingExpenseRatio { get; init; }

    public static ProfitabilityResponse From(Profitability profitability)
    {
        return new ProfitabilityResponse
        {
            GrossProfitMargin = profitability.GrossProfitMargin,
            OperatingProfitMargin = profitability.OperatingProfitMargin,
            OrdinaryProfitMargin = profitability.OrdinaryProfitMargin,
            SellingExpenseRatio = profitability.SellingExpenseRatio
        };
    }
}

/// <summary>
/// 効率性指標レスポンス
/// </summary>
public record EfficiencyResponse
{
    /// <summary>
    /// 総資本回転率（回）
    /// </summary>
    public required decimal TotalAssetTurnover { get; init; }

    /// <summary>
    /// 売上債権回転率（回）
    /// </summary>
    public required decimal ReceivablesTurnover { get; init; }

    /// <summary>
    /// 棚卸資産回転率（回）
    /// </summary>
    public required decimal InventoryTurnover { get; init; }

    /// <summary>
    /// 有形固定資産回転率（回）
    /// </summary>
    public required decimal TangibleFixedAssetTurnover { get; init; }

    public static EfficiencyResponse From(Efficiency efficiency)
    {
        return new EfficiencyResponse
        {
            TotalAssetTurnover = efficiency.TotalAssetTurnover,
            ReceivablesTurnover = efficiency.ReceivablesTurnover,
            InventoryTurnover = efficiency.InventoryTurnover,
            TangibleFixedAssetTurnover = efficiency.TangibleFixedAssetTurnover
        };
    }
}

/// <summary>
/// 安全性指標レスポンス
/// </summary>
public record SafetyResponse
{
    /// <summary>
    /// 流動比率（%）
    /// </summary>
    public required decimal CurrentRatio { get; init; }

    /// <summary>
    /// 当座比率（%）
    /// </summary>
    public required decimal QuickRatio { get; init; }

    /// <summary>
    /// 固定比率（%）
    /// </summary>
    public required decimal FixedRatio { get; init; }

    /// <summary>
    /// 固定長期適合率（%）
    /// </summary>
    public required decimal FixedLongTermRatio { get; init; }

    /// <summary>
    /// 負債比率（%）
    /// </summary>
    public required decimal DebtRatio { get; init; }

    /// <summary>
    /// 自己資本比率（%）
    /// </summary>
    public required decimal EquityRatio { get; init; }

    public static SafetyResponse From(Safety safety)
    {
        return new SafetyResponse
        {
            CurrentRatio = safety.CurrentRatio,
            QuickRatio = safety.QuickRatio,
            FixedRatio = safety.FixedRatio,
            FixedLongTermRatio = safety.FixedLongTermRatio,
            DebtRatio = safety.DebtRatio,
            EquityRatio = safety.EquityRatio
        };
    }
}
