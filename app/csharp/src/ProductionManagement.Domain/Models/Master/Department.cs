namespace ProductionManagement.Domain.Models.Master;

/// <summary>
/// 部門
/// </summary>
public class Department
{
    public required string DepartmentCode { get; init; }
    public required string DepartmentName { get; init; }
    public DateTime CreatedAt { get; init; }
    public DateTime UpdatedAt { get; init; }
}
