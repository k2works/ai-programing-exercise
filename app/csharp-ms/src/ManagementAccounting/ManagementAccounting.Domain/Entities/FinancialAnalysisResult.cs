namespace ManagementAccounting.Domain.Entities;

/// <summary>
/// 財務分析結果（管理会計コンテキストのドメインモデル）
/// </summary>
public record FinancialAnalysisResult(
    FinancialData Data,
    FinancialRatios Ratios,
    DateTime AnalyzedAt
)
{
    public FinancialAnalysisResult(FinancialData data, FinancialRatios ratios)
        : this(data, ratios, DateTime.UtcNow)
    {
    }
}
