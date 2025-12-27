namespace ProductionManagement.Domain.Models.Process;

/// <summary>
/// 工程表
/// </summary>
public class Routing
{
    public int Id { get; set; }
    public required string ItemCode { get; set; }
    public required int Sequence { get; set; }
    public required string ProcessCode { get; set; }
    public DateTime? CreatedAt { get; set; }
    public DateTime? UpdatedAt { get; set; }
}
