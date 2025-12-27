namespace ProductionManagement.Application.Port.In.Dto;

/// <summary>
/// 工数サマリ
/// </summary>
public class LaborHoursSummary
{
    public required decimal TotalHours { get; init; }
    public required IReadOnlyList<ProcessLaborHours> ProcessHours { get; init; }
}

/// <summary>
/// 工程別工数
/// </summary>
public class ProcessLaborHours
{
    public required string ProcessCode { get; init; }
    public required string ProcessName { get; init; }
    public required decimal Hours { get; init; }
}
