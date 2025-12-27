namespace ProductionManagement.Domain.Models.Process;

/// <summary>
/// 完成検査結果データ
/// </summary>
public class CompletionInspectionResult
{
    public int Id { get; set; }
    public required string CompletionResultNumber { get; set; }
    public required string DefectCode { get; set; }
    public required decimal Quantity { get; set; }
    public DateTime? CreatedAt { get; set; }
    public DateTime? UpdatedAt { get; set; }
}
