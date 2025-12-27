namespace ProductionManagement.Domain.Models.Inventory;

/// <summary>
/// 在庫情報
/// </summary>
public class Stock
{
    public long? Id { get; init; }
    public required string LocationCode { get; init; }
    public required string ItemCode { get; init; }
    public decimal StockQuantity { get; init; }
    public decimal PassedQuantity { get; init; }
    public decimal DefectiveQuantity { get; init; }
    public decimal UninspectedQuantity { get; init; }
    public DateTime CreatedAt { get; init; }
    public DateTime UpdatedAt { get; init; }

    public static Stock Empty(string locationCode, string itemCode) => new()
    {
        LocationCode = locationCode,
        ItemCode = itemCode,
        StockQuantity = 0m,
        PassedQuantity = 0m,
        DefectiveQuantity = 0m,
        UninspectedQuantity = 0m
    };
}
