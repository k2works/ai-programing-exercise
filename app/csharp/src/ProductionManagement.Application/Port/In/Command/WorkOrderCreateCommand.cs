namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 作業指示作成コマンド
/// </summary>
public class WorkOrderCreateCommand
{
    public required string OrderNumber { get; init; }
    public required DateOnly WorkOrderDate { get; init; }
    public required string LocationCode { get; init; }
    public required DateOnly PlannedStartDate { get; init; }
    public required DateOnly PlannedEndDate { get; init; }
    public string? Remarks { get; init; }
}
