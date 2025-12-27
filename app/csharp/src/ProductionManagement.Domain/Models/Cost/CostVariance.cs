namespace ProductionManagement.Domain.Models.Cost;

/// <summary>
/// 原価差異データ
/// </summary>
public class CostVariance
{
    public long? Id { get; init; }

    public required string WorkOrderNumber { get; init; }

    public required string ItemCode { get; init; }

    public required decimal MaterialCostVariance { get; init; }

    public required decimal LaborCostVariance { get; init; }

    public required decimal ExpenseVariance { get; init; }

    public required decimal TotalVariance { get; init; }

    public DateTime CreatedAt { get; init; }
}
