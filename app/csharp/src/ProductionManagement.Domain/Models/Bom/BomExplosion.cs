namespace ProductionManagement.Domain.Models.Bom;

/// <summary>
/// BOM 展開結果
/// </summary>
public class BomExplosion
{
    public required string ParentItemCode { get; set; }

    public required string ChildItemCode { get; set; }

    public DateOnly EffectiveFrom { get; set; }

    public DateOnly? EffectiveTo { get; set; }

    public decimal BaseQuantity { get; set; }

    public decimal RequiredQuantity { get; set; }

    public decimal DefectRate { get; set; }

    public int? Sequence { get; set; }

    public int Level { get; set; }

    public decimal TotalQuantity { get; set; }
}
