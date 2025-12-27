namespace ProductionManagement.Domain.Models.Inventory;

/// <summary>
/// 棚卸ステータス
/// </summary>
public enum StocktakingStatus
{
    /// <summary>発行済</summary>
    Issued,

    /// <summary>入力済</summary>
    Entered,

    /// <summary>確定</summary>
    Confirmed
}

/// <summary>
/// 棚卸ステータス拡張メソッド
/// </summary>
public static class StocktakingStatusExtensions
{
    private static readonly Dictionary<StocktakingStatus, string> DisplayNames = new()
    {
        { StocktakingStatus.Issued, "発行済" },
        { StocktakingStatus.Entered, "入力済" },
        { StocktakingStatus.Confirmed, "確定" }
    };

    public static string ToDisplayName(this StocktakingStatus status)
        => DisplayNames[status];

    public static StocktakingStatus FromDisplayName(string displayName)
        => DisplayNames.First(x => x.Value == displayName).Key;
}
