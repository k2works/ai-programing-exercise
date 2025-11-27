namespace AccountingSystem.Domain.Models.Financial;

/// <summary>
/// 財務分析結果を表現する値オブジェクト
/// </summary>
public record AnalysisResult(
    int FiscalYear,
    Profitability Profitability,
    Efficiency Efficiency,
    Safety Safety
);

/// <summary>
/// 収益性指標
/// </summary>
public record Profitability(
    /// <summary>
    /// 売上高総利益率（%）
    /// 売上総利益 / 売上高 × 100
    /// </summary>
    decimal GrossProfitMargin,

    /// <summary>
    /// 売上高営業利益率（%）
    /// 営業利益 / 売上高 × 100
    /// </summary>
    decimal OperatingProfitMargin,

    /// <summary>
    /// 売上高経常利益率（%）
    /// 経常利益 / 売上高 × 100
    /// </summary>
    decimal OrdinaryProfitMargin,

    /// <summary>
    /// 売上高販管費比率（%）
    /// 販管費 / 売上高 × 100
    /// </summary>
    decimal SellingExpenseRatio
);

/// <summary>
/// 効率性指標
/// </summary>
public record Efficiency(
    /// <summary>
    /// 総資本回転率（回）
    /// 売上高 / 総資本
    /// </summary>
    decimal TotalAssetTurnover,

    /// <summary>
    /// 売上債権回転率（回）
    /// 売上高 / 売上債権
    /// </summary>
    decimal ReceivablesTurnover,

    /// <summary>
    /// 棚卸資産回転率（回）
    /// 売上高 / 棚卸資産
    /// </summary>
    decimal InventoryTurnover,

    /// <summary>
    /// 有形固定資産回転率（回）
    /// 売上高 / 有形固定資産
    /// </summary>
    decimal TangibleFixedAssetTurnover
);

/// <summary>
/// 安全性指標
/// </summary>
public record Safety(
    /// <summary>
    /// 流動比率（%）
    /// 流動資産 / 流動負債 × 100
    /// </summary>
    decimal CurrentRatio,

    /// <summary>
    /// 当座比率（%）
    /// 当座資産 / 流動負債 × 100
    /// </summary>
    decimal QuickRatio,

    /// <summary>
    /// 固定比率（%）
    /// 固定資産 / 自己資本 × 100
    /// </summary>
    decimal FixedRatio,

    /// <summary>
    /// 固定長期適合率（%）
    /// 固定資産 / (自己資本 + 固定負債) × 100
    /// </summary>
    decimal FixedLongTermRatio,

    /// <summary>
    /// 負債比率（%）
    /// 負債 / 自己資本 × 100
    /// </summary>
    decimal DebtRatio,

    /// <summary>
    /// 自己資本比率（%）
    /// 自己資本 / 総資本 × 100
    /// </summary>
    decimal EquityRatio
);
