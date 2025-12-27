namespace ProductionManagement.Domain.Models.Process;

/// <summary>
/// 完成実績データ
/// </summary>
public class CompletionResult
{
    public int Id { get; set; }
    public required string CompletionResultNumber { get; set; }
    public required string WorkOrderNumber { get; set; }
    public required string ItemCode { get; set; }
    public required DateOnly CompletionDate { get; set; }
    public required decimal CompletedQuantity { get; set; }
    public required decimal GoodQuantity { get; set; }
    public required decimal DefectQuantity { get; set; }
    public string? Remarks { get; set; }
    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }

    public IReadOnlyList<CompletionInspectionResult> InspectionResults { get; set; } = [];
}
