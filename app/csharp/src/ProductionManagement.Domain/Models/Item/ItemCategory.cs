namespace ProductionManagement.Domain.Models.Item;

/// <summary>
/// 品目区分
/// </summary>
public enum ItemCategory
{
    /// <summary>製品</summary>
    Product,
    /// <summary>半製品</summary>
    SemiProduct,
    /// <summary>中間品</summary>
    Intermediate,
    /// <summary>部品</summary>
    Part,
    /// <summary>材料</summary>
    Material,
    /// <summary>原料</summary>
    RawMaterial,
    /// <summary>資材</summary>
    Supply
}

/// <summary>
/// ItemCategory 拡張メソッド
/// </summary>
public static class ItemCategoryExtensions
{
    private static readonly Dictionary<ItemCategory, string> DisplayNames = new()
    {
        { ItemCategory.Product, "製品" },
        { ItemCategory.SemiProduct, "半製品" },
        { ItemCategory.Intermediate, "中間品" },
        { ItemCategory.Part, "部品" },
        { ItemCategory.Material, "材料" },
        { ItemCategory.RawMaterial, "原料" },
        { ItemCategory.Supply, "資材" }
    };

    private static readonly Dictionary<string, ItemCategory> FromDisplayNames =
        DisplayNames.ToDictionary(x => x.Value, x => x.Key);

    public static string GetDisplayName(this ItemCategory category)
    {
        return DisplayNames[category];
    }

    public static ItemCategory FromDisplayName(string displayName)
    {
        if (FromDisplayNames.TryGetValue(displayName, out var category))
        {
            return category;
        }
        throw new ArgumentException($"不正な品目区分: {displayName}");
    }
}
