namespace ProductionManagement.Domain.Models.Cost;

/// <summary>
/// 実際原価データ
/// </summary>
public class ActualCost
{
    public long? Id { get; init; }

    public required string WorkOrderNumber { get; init; }

    public required string ItemCode { get; init; }

    public required decimal CompletedQuantity { get; init; }

    public required decimal ActualMaterialCost { get; init; }

    public required decimal ActualLaborCost { get; init; }

    public required decimal ActualExpense { get; init; }

    public required decimal ActualManufacturingCost { get; init; }

    public required decimal UnitCost { get; init; }

    public DateTime CreatedAt { get; init; }

    public DateTime UpdatedAt { get; init; }
}
