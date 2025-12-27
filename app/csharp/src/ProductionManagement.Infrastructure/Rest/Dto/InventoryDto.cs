using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Infrastructure.Rest.Dto;

/// <summary>
/// 在庫レスポンス
/// </summary>
public record StockResponse(
    long? Id,
    string LocationCode,
    string ItemCode,
    decimal StockQuantity,
    decimal PassedQuantity,
    decimal DefectiveQuantity,
    decimal UninspectedQuantity
)
{
    public static StockResponse From(Stock stock) => new(
        Id: stock.Id,
        LocationCode: stock.LocationCode,
        ItemCode: stock.ItemCode,
        StockQuantity: stock.StockQuantity,
        PassedQuantity: stock.PassedQuantity,
        DefectiveQuantity: stock.DefectiveQuantity,
        UninspectedQuantity: stock.UninspectedQuantity
    );
}

/// <summary>
/// 在庫サマリーレスポンス
/// </summary>
public record InventorySummaryResponse(
    string ItemCode,
    string ItemName,
    decimal TotalQuantity,
    decimal? SafetyStock,
    string StockState
)
{
    public static InventorySummaryResponse From(InventorySummary summary) => new(
        ItemCode: summary.ItemCode,
        ItemName: summary.ItemName,
        TotalQuantity: summary.TotalQuantity,
        SafetyStock: summary.SafetyStock,
        StockState: summary.StockState.ToString()
    );
}
