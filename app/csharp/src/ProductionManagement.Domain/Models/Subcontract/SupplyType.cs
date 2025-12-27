namespace ProductionManagement.Domain.Models.Subcontract;

/// <summary>
/// 支給区分
/// </summary>
public enum SupplyType
{
    /// <summary>有償支給</summary>
    Paid,
    /// <summary>無償支給</summary>
    Free
}

/// <summary>
/// SupplyType 拡張メソッド
/// </summary>
public static class SupplyTypeExtensions
{
    private static readonly Dictionary<SupplyType, string> DisplayNames = new()
    {
        { SupplyType.Paid, "有償支給" },
        { SupplyType.Free, "無償支給" }
    };

    private static readonly Dictionary<string, SupplyType> FromDisplayNames =
        DisplayNames.ToDictionary(x => x.Value, x => x.Key);

    public static string GetDisplayName(this SupplyType type) => DisplayNames[type];

    public static SupplyType FromDisplayName(string displayName)
    {
        if (FromDisplayNames.TryGetValue(displayName, out var type))
        {
            return type;
        }
        throw new ArgumentException($"Unknown supply type: {displayName}");
    }
}
