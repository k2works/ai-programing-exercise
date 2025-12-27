using ProductionManagement.Domain.Models.Item;

namespace ProductionManagement.Application.Port.In;

/// <summary>
/// 品目登録コマンド
/// </summary>
public record CreateItemCommand(
    string ItemCode,
    string ItemName,
    ItemCategory Category,
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
/// 品目更新コマンド
/// </summary>
public record UpdateItemCommand(
    string ItemCode,
    string? ItemName = null,
    ItemCategory? Category = null,
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
