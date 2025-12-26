namespace ProductionManagement.Domain.Models.Bom;

/// <summary>
/// 部品構成表（BOM）
/// </summary>
public class Bom
{
    public required string ParentItemCode { get; set; }

    public required string ChildItemCode { get; set; }

    public DateOnly EffectiveFrom { get; set; }

    public DateOnly? EffectiveTo { get; set; }

    public decimal BaseQuantity { get; set; } = 1m;

    public decimal RequiredQuantity { get; set; }

    public decimal DefectRate { get; set; } = 0m;

    public int? Sequence { get; set; }

    public DateTime CreatedAt { get; set; }

    public DateTime UpdatedAt { get; set; }
}
