namespace ProductionManagement.Domain.Models.Master;

/// <summary>
/// 担当者
/// </summary>
public class Employee
{
    public required string EmployeeCode { get; init; }
    public required string EmployeeName { get; init; }
    public required string DepartmentCode { get; init; }
    public DateTime CreatedAt { get; init; }
    public DateTime UpdatedAt { get; init; }

    // リレーション
    public Department? Department { get; set; }
}
