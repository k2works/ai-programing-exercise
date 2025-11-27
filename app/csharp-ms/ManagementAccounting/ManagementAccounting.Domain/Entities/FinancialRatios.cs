namespace ManagementAccounting.Domain.Entities;

/// <summary>
/// 財務比率（管理会計コンテキストのドメインモデル）
///
/// 収益性・効率性・安全性の各指標を計算した結果
/// </summary>
public record FinancialRatios(
    /// <summary>営業利益率 = 営業利益 / 売上高</summary>
    decimal OperatingProfitMargin,

    /// <summary>総資産回転率 = 売上高 / 総資産</summary>
    decimal TotalAssetTurnover,

    /// <summary>固定資産回転率 = 売上高 / 有形固定資産</summary>
    decimal FixedAssetTurnover,

    /// <summary>流動比率 = 流動資産 / 流動負債 * 100</summary>
    decimal CurrentRatio,

    /// <summary>当座比率 = 当座資産 / 流動負債 * 100</summary>
    decimal QuickRatio,

    /// <summary>自己資本比率 = 自己資本 / 総資産 * 100</summary>
    decimal EquityRatio
);
