namespace ProductionManagement.Domain.Models.Process;

/// <summary>
/// 作業指示明細データ
/// </summary>
public class WorkOrderDetail
{
    public int Id { get; set; }
    public required string WorkOrderNumber { get; set; }
    public required int Sequence { get; set; }
    public required string ProcessCode { get; set; }
    public DateTime? CreatedAt { get; set; }
    public DateTime? UpdatedAt { get; set; }
}
