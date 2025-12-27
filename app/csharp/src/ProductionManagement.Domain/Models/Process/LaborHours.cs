using ProductionManagement.Domain.Models.Master;

namespace ProductionManagement.Domain.Models.Process;

/// <summary>
/// 工数実績
/// </summary>
public class LaborHours
{
    public int Id { get; set; }
    public required string LaborHoursNumber { get; set; }
    public required string WorkOrderNumber { get; set; }
    public required string ItemCode { get; set; }
    public required int Sequence { get; set; }
    public required string ProcessCode { get; set; }
    public required string DepartmentCode { get; set; }
    public required string EmployeeCode { get; set; }
    public required DateOnly WorkDate { get; set; }
    public required decimal Hours { get; set; }
    public string? Remarks { get; set; }
    public DateTime CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }

    // リレーション
    public WorkOrder? WorkOrder { get; set; }
    public Item.Item? Item { get; set; }
    public Process? Process { get; set; }
    public Department? Department { get; set; }
    public Employee? Employee { get; set; }
}
