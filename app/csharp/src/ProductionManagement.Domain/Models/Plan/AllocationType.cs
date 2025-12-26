namespace ProductionManagement.Domain.Models.Plan;

/// <summary>
/// 引当区分
/// </summary>
public enum AllocationType
{
    /// <summary>在庫</summary>
    Inventory,
    /// <summary>発注残</summary>
    PurchaseOrder,
    /// <summary>製造残</summary>
    ManufacturingOrder
}

/// <summary>
/// AllocationType 拡張メソッド
/// </summary>
public static class AllocationTypeExtensions
{
    private static readonly Dictionary<AllocationType, string> DisplayNames = new()
    {
        { AllocationType.Inventory, "在庫" },
        { AllocationType.PurchaseOrder, "発注残" },
        { AllocationType.ManufacturingOrder, "製造残" }
    };

    private static readonly Dictionary<string, AllocationType> FromDisplayNames =
        DisplayNames.ToDictionary(x => x.Value, x => x.Key);

    public static string GetDisplayName(this AllocationType type) => DisplayNames[type];

    public static AllocationType FromDisplayName(string displayName)
    {
        if (FromDisplayNames.TryGetValue(displayName, out var type))
        {
            return type;
        }
        throw new ArgumentException($"Unknown allocation type: {displayName}");
    }
}
