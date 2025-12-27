namespace ProductionManagement.Domain.Models.Inventory;

/// <summary>
/// 棚卸明細データ
/// </summary>
public class StocktakingDetail
{
    public long? Id { get; init; }
    public required string StocktakingNumber { get; init; }
    public required int LineNumber { get; init; }
    public required string ItemCode { get; init; }
    public required decimal BookQuantity { get; init; }
    public decimal? ActualQuantity { get; init; }
    public decimal? DifferenceQuantity { get; init; }
    public DateTime CreatedAt { get; init; }
    public DateTime UpdatedAt { get; init; }
}
