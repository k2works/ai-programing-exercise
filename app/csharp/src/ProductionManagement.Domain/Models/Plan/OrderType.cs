namespace ProductionManagement.Domain.Models.Plan;

/// <summary>
/// オーダ種別
/// </summary>
public enum OrderType
{
    /// <summary>購買</summary>
    Purchase,
    /// <summary>製造</summary>
    Manufacturing
}

/// <summary>
/// OrderType 拡張メソッド
/// </summary>
public static class OrderTypeExtensions
{
    private static readonly Dictionary<OrderType, string> DisplayNames = new()
    {
        { OrderType.Purchase, "購買" },
        { OrderType.Manufacturing, "製造" }
    };

    private static readonly Dictionary<string, OrderType> FromDisplayNames =
        DisplayNames.ToDictionary(x => x.Value, x => x.Key);

    public static string GetDisplayName(this OrderType type) => DisplayNames[type];

    public static OrderType FromDisplayName(string displayName)
    {
        if (FromDisplayNames.TryGetValue(displayName, out var type))
        {
            return type;
        }
        throw new ArgumentException($"Unknown order type: {displayName}");
    }
}
