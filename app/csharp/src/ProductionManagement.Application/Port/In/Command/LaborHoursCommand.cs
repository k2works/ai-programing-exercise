namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 工数実績報告コマンド
/// </summary>
public class LaborHoursCommand
{
    public required string WorkOrderNumber { get; init; }
    public required int Sequence { get; init; }
    public required DateOnly WorkDate { get; init; }
    public required string EmployeeCode { get; init; }
    public required string DepartmentCode { get; init; }
    public required decimal Hours { get; init; }
    public string? Remarks { get; init; }
}
