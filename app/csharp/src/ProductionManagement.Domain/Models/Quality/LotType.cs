namespace ProductionManagement.Domain.Models.Quality;

/// <summary>
/// ロット種別
/// </summary>
public enum LotType
{
    Purchased,
    Manufactured
}

/// <summary>
/// ロット種別の拡張メソッド
/// </summary>
public static class LotTypeExtensions
{
    private static readonly Dictionary<LotType, string> DisplayNames = new()
    {
        { LotType.Purchased, "購入ロット" },
        { LotType.Manufactured, "製造ロット" }
    };

    public static string ToDisplayName(this LotType type)
        => DisplayNames[type];

    public static LotType FromDisplayName(string displayName)
        => DisplayNames.First(x => x.Value == displayName).Key;
}
