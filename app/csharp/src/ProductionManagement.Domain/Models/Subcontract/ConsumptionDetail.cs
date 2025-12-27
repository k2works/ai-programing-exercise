namespace ProductionManagement.Domain.Models.Subcontract;

/// <summary>
/// 消費明細データ
/// </summary>
public class ConsumptionDetail
{
    public int Id { get; set; }
    public required string ConsumptionNumber { get; set; }
    public required int LineNumber { get; set; }
    public required string ItemCode { get; set; }
    public required decimal Quantity { get; set; }
    public string? Remarks { get; set; }
    public DateTime? CreatedAt { get; set; }
    public DateTime? UpdatedAt { get; set; }
}
