namespace AccountingSystem.Domain.Models.Financial;

/// <summary>
/// 貸借対照表（Balance Sheet / B/S）
/// </summary>
public record BalanceSheet
{
    /// <summary>
    /// 基準日
    /// </summary>
    public required DateOnly AsOfDate { get; init; }

    /// <summary>
    /// 資産項目
    /// </summary>
    public required IReadOnlyList<BalanceSheetItem> Assets { get; init; }

    /// <summary>
    /// 負債項目
    /// </summary>
    public required IReadOnlyList<BalanceSheetItem> Liabilities { get; init; }

    /// <summary>
    /// 純資産項目
    /// </summary>
    public required IReadOnlyList<BalanceSheetItem> Equity { get; init; }

    /// <summary>
    /// 資産合計
    /// </summary>
    public required decimal TotalAssets { get; init; }

    /// <summary>
    /// 負債合計
    /// </summary>
    public required decimal TotalLiabilities { get; init; }

    /// <summary>
    /// 純資産合計
    /// </summary>
    public required decimal TotalEquity { get; init; }

    /// <summary>
    /// 負債・純資産合計
    /// </summary>
    public required decimal TotalLiabilitiesAndEquity { get; init; }
}
