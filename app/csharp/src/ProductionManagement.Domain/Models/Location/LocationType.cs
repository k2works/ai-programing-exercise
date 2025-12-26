namespace ProductionManagement.Domain.Models.Location;

/// <summary>
/// 場所区分
/// </summary>
public enum LocationType
{
    /// <summary>倉庫</summary>
    Warehouse,

    /// <summary>製造</summary>
    Manufacturing,

    /// <summary>検査</summary>
    Inspection,

    /// <summary>出荷</summary>
    Shipping,

    /// <summary>外注</summary>
    Subcontract
}

/// <summary>
/// LocationType 拡張メソッド
/// </summary>
public static class LocationTypeExtensions
{
    private static readonly Dictionary<LocationType, string> DisplayNames = new()
    {
        { LocationType.Warehouse, "倉庫" },
        { LocationType.Manufacturing, "製造" },
        { LocationType.Inspection, "検査" },
        { LocationType.Shipping, "出荷" },
        { LocationType.Subcontract, "外注" }
    };

    private static readonly Dictionary<string, LocationType> FromDisplayNames =
        DisplayNames.ToDictionary(x => x.Value, x => x.Key);

    public static string GetDisplayName(this LocationType locationType)
    {
        return DisplayNames[locationType];
    }

    public static LocationType FromDisplayName(string displayName)
    {
        if (FromDisplayNames.TryGetValue(displayName, out var locationType))
        {
            return locationType;
        }

        throw new ArgumentException($"不正な場所区分: {displayName}");
    }
}
