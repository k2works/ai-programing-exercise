namespace ProductionManagement.Domain.Models.Process;

/// <summary>
/// 工程マスタ
/// </summary>
public class Process
{
    public required string ProcessCode { get; set; }
    public required string ProcessName { get; set; }
    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }
}
