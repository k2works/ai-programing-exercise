namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 消費作成コマンド
/// </summary>
public class ConsumptionCommand
{
    public required string ReceivingNumber { get; init; }
    public required DateOnly ConsumptionDate { get; init; }
    public required string SupplierCode { get; init; }
    public string? Remarks { get; init; }
    public string? CreatedBy { get; init; }
    public required IReadOnlyList<ConsumptionDetailCommand> Details { get; init; }
}

/// <summary>
/// 消費明細コマンド
/// </summary>
public class ConsumptionDetailCommand
{
    public required string ItemCode { get; init; }
    public required decimal Quantity { get; init; }
    public string? Remarks { get; init; }
}
