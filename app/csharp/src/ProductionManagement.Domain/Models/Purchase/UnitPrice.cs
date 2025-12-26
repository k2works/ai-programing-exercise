namespace ProductionManagement.Domain.Models.Purchase;

/// <summary>
/// 単価マスタ
/// </summary>
public class UnitPrice
{
    public int Id { get; set; }
    public required string ItemCode { get; set; }
    public required string SupplierCode { get; set; }
    public decimal LotUnitQuantity { get; set; } = 1m;
    public required DateOnly EffectiveFrom { get; set; }
    public DateOnly? EffectiveTo { get; set; }
    public required decimal Price { get; set; }
    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }
}
