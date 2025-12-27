namespace ProductionManagement.Domain.Models.Cost;

/// <summary>
/// 標準原価マスタ
/// </summary>
public class StandardCost
{
    public long? Id { get; init; }

    public required string ItemCode { get; init; }

    public required DateOnly EffectiveStartDate { get; init; }

    public DateOnly? EffectiveEndDate { get; init; }

    public required decimal StandardMaterialCost { get; init; }

    public required decimal StandardLaborCost { get; init; }

    public required decimal StandardExpense { get; init; }

    public required decimal StandardManufacturingCost { get; init; }

    public DateTime CreatedAt { get; init; }

    public DateTime UpdatedAt { get; init; }
}
