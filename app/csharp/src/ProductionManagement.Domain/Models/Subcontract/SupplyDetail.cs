namespace ProductionManagement.Domain.Models.Subcontract;

/// <summary>
/// 支給明細データ
/// </summary>
public class SupplyDetail
{
    public int Id { get; set; }
    public required string SupplyNumber { get; set; }
    public required int LineNumber { get; set; }
    public required string ItemCode { get; set; }
    public required decimal Quantity { get; set; }
    public required decimal UnitPrice { get; set; }
    public required decimal Amount { get; set; }
    public string? Remarks { get; set; }
    public DateTime? CreatedAt { get; set; }
    public DateTime? UpdatedAt { get; set; }
}
