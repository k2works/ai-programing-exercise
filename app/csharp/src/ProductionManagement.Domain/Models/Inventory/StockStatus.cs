namespace ProductionManagement.Domain.Models.Inventory;

/// <summary>
/// 在庫状態
/// </summary>
public enum StockStatus
{
    Passed,
    Defective,
    Uninspected
}

public static class StockStatusExtensions
{
    private static readonly Dictionary<StockStatus, string> DisplayNames = new()
    {
        { StockStatus.Passed, "合格" },
        { StockStatus.Defective, "不良" },
        { StockStatus.Uninspected, "未検査" }
    };

    public static string ToDisplayName(this StockStatus status)
        => DisplayNames[status];

    public static StockStatus FromDisplayName(string displayName)
        => DisplayNames.First(x => x.Value == displayName).Key;
}
