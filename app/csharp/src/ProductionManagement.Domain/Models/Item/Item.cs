namespace ProductionManagement.Domain.Models.Item;

/// <summary>
/// 品目マスタ
/// </summary>
public class Item
{
    private string _itemCategoryValue = "製品";

    public int Id { get; set; }

    public required string ItemCode { get; set; }

    public DateOnly EffectiveFrom { get; set; }

    public DateOnly? EffectiveTo { get; set; }

    public required string ItemName { get; set; }

    /// <summary>
    /// 品目区分（データベース格納用の日本語文字列）
    /// Dapper のマッピングに使用
    /// </summary>
    public string ItemCategoryValue
    {
        get => _itemCategoryValue;
        set => _itemCategoryValue = value;
    }

    /// <summary>
    /// 品目区分（ドメインロジック用の Enum）
    /// </summary>
    public ItemCategory ItemCategory
    {
        get => ItemCategoryExtensions.FromDisplayName(_itemCategoryValue);
        set => _itemCategoryValue = value.GetDisplayName();
    }

    public string? UnitCode { get; set; }

    public int LeadTime { get; set; }

    public int SafetyLeadTime { get; set; }

    public decimal SafetyStock { get; set; } = 0m;

    public decimal YieldRate { get; set; } = 100m;

    public decimal MinLotSize { get; set; } = 1m;

    public decimal LotIncrement { get; set; } = 1m;

    public decimal? MaxLotSize { get; set; }

    public int? ShelfLife { get; set; }

    public DateTime CreatedAt { get; set; }

    public DateTime UpdatedAt { get; set; }
}
