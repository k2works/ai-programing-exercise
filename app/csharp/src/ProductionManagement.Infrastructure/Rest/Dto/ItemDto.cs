using System.ComponentModel.DataAnnotations;
using ProductionManagement.Domain.Models.Item;

namespace ProductionManagement.Infrastructure.Rest.Dto;

/// <summary>
/// 品目登録リクエスト
/// </summary>
public record CreateItemRequest(
    [Required(ErrorMessage = "品目コードは必須です")]
    [StringLength(20, ErrorMessage = "品目コードは20文字以内で入力してください")]
    string ItemCode,

    [Required(ErrorMessage = "品名は必須です")]
    [StringLength(100, ErrorMessage = "品名は100文字以内で入力してください")]
    string ItemName,

    [Required(ErrorMessage = "品目区分は必須です")]
    string Category,

    string? UnitCode = null,
    int LeadTime = 0,
    int SafetyLeadTime = 0,
    decimal SafetyStock = 0m,
    decimal YieldRate = 100m,
    decimal MinLotSize = 1m,
    decimal LotIncrement = 1m,
    decimal? MaxLotSize = null,
    int? ShelfLife = null
);

/// <summary>
/// 品目更新リクエスト
/// </summary>
public record UpdateItemRequest(
    string? ItemName = null,
    string? Category = null,
    string? UnitCode = null,
    int? LeadTime = null,
    int? SafetyLeadTime = null,
    decimal? SafetyStock = null,
    decimal? YieldRate = null,
    decimal? MinLotSize = null,
    decimal? LotIncrement = null,
    decimal? MaxLotSize = null,
    int? ShelfLife = null
);

/// <summary>
/// 品目レスポンス
/// </summary>
public record ItemResponse(
    string ItemCode,
    DateOnly EffectiveFrom,
    DateOnly? EffectiveTo,
    string ItemName,
    string Category,
    string? UnitCode,
    int LeadTime,
    int SafetyLeadTime,
    decimal SafetyStock,
    decimal YieldRate,
    decimal MinLotSize,
    decimal LotIncrement,
    decimal? MaxLotSize,
    int? ShelfLife
)
{
    public static ItemResponse From(Item item) => new(
        ItemCode: item.ItemCode,
        EffectiveFrom: item.EffectiveFrom,
        EffectiveTo: item.EffectiveTo,
        ItemName: item.ItemName,
        Category: item.ItemCategory.ToString(),
        UnitCode: item.UnitCode,
        LeadTime: item.LeadTime,
        SafetyLeadTime: item.SafetyLeadTime,
        SafetyStock: item.SafetyStock,
        YieldRate: item.YieldRate,
        MinLotSize: item.MinLotSize,
        LotIncrement: item.LotIncrement,
        MaxLotSize: item.MaxLotSize,
        ShelfLife: item.ShelfLife
    );
}
