namespace AccountingSystem.Domain.Models;

/// <summary>
/// 財務指標
/// </summary>
public record FinancialRatios
{
    /// <summary>
    /// 流動比率（%）
    /// 流動資産 / 流動負債 × 100
    /// 短期的な支払い能力を示す（200%以上が望ましい）
    /// </summary>
    public required decimal CurrentRatio { get; init; }

    /// <summary>
    /// 自己資本比率（%）
    /// 純資産 / 総資産 × 100
    /// 財務の安全性を示す（高いほど安全）
    /// </summary>
    public required decimal EquityRatio { get; init; }

    /// <summary>
    /// 売上総利益率（%）
    /// 売上総利益 / 売上高 × 100
    /// 商品・サービスの粗利率
    /// </summary>
    public required decimal GrossProfitMargin { get; init; }

    /// <summary>
    /// 営業利益率（%）
    /// 営業利益 / 売上高 × 100
    /// 本業の収益性
    /// </summary>
    public required decimal OperatingProfitMargin { get; init; }

    /// <summary>
    /// 当期純利益率（%）
    /// 当期純利益 / 売上高 × 100
    /// 最終的な収益性
    /// </summary>
    public required decimal NetProfitMargin { get; init; }

    /// <summary>
    /// 総資産利益率（ROA, %）
    /// 当期純利益 / 総資産 × 100
    /// 経営効率を示す
    /// </summary>
    public required decimal Roa { get; init; }

    /// <summary>
    /// 自己資本利益率（ROE, %）
    /// 当期純利益 / 純資産 × 100
    /// 株主への還元率を示す
    /// </summary>
    public required decimal Roe { get; init; }
}
