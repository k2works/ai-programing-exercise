namespace ProductionManagement.Domain.Models.Purchase;

/// <summary>
/// 入荷受入区分
/// </summary>
public enum ReceivingType
{
    /// <summary>通常入荷</summary>
    Normal,
    /// <summary>分割入荷</summary>
    Split,
    /// <summary>返品入荷</summary>
    Return
}

/// <summary>
/// ReceivingType 拡張メソッド
/// </summary>
public static class ReceivingTypeExtensions
{
    private static readonly Dictionary<ReceivingType, string> DisplayNames = new()
    {
        { ReceivingType.Normal, "通常入荷" },
        { ReceivingType.Split, "分割入荷" },
        { ReceivingType.Return, "返品入荷" }
    };

    private static readonly Dictionary<string, ReceivingType> FromDisplayNames =
        DisplayNames.ToDictionary(x => x.Value, x => x.Key);

    public static string GetDisplayName(this ReceivingType type) => DisplayNames[type];

    public static ReceivingType FromDisplayName(string displayName)
    {
        if (FromDisplayNames.TryGetValue(displayName, out var type))
        {
            return type;
        }
        throw new ArgumentException($"Unknown receiving type: {displayName}");
    }
}
