namespace ProductionManagement.Domain.Models.Calendar;

/// <summary>
/// 日付区分
/// </summary>
public enum DateType
{
    /// <summary>稼働日</summary>
    Working,

    /// <summary>休日</summary>
    Holiday,

    /// <summary>半日稼働</summary>
    HalfDay
}

/// <summary>
/// DateType 拡張メソッド
/// </summary>
public static class DateTypeExtensions
{
    private static readonly Dictionary<DateType, string> DisplayNames = new()
    {
        { DateType.Working, "稼働日" },
        { DateType.Holiday, "休日" },
        { DateType.HalfDay, "半日稼働" }
    };

    private static readonly Dictionary<string, DateType> FromDisplayNames =
        DisplayNames.ToDictionary(x => x.Value, x => x.Key);

    public static string GetDisplayName(this DateType dateType)
    {
        return DisplayNames[dateType];
    }

    public static DateType FromDisplayName(string displayName)
    {
        if (FromDisplayNames.TryGetValue(displayName, out var dateType))
        {
            return dateType;
        }

        throw new ArgumentException($"不正な日付区分: {displayName}");
    }
}
