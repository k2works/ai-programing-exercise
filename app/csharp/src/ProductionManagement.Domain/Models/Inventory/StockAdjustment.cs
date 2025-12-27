namespace ProductionManagement.Domain.Models.Inventory;

/// <summary>
/// 在庫調整データ
/// </summary>
public class StockAdjustment
{
    public long? Id { get; init; }
    public required string AdjustmentNumber { get; init; }
    public string? StocktakingNumber { get; init; }
    public required string ItemCode { get; init; }
    public required string LocationCode { get; init; }
    public required DateOnly AdjustmentDate { get; init; }
    public required string AdjusterCode { get; init; }
    public required decimal AdjustmentQuantity { get; init; }
    public required string ReasonCode { get; init; }
    public DateTime CreatedAt { get; init; }
    public DateTime UpdatedAt { get; init; }
}
