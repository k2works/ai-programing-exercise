namespace ProductionManagement.Domain.Models.Purchase;

/// <summary>
/// 検収データ
/// </summary>
public class Acceptance
{
    public int Id { get; set; }
    public required string AcceptanceNumber { get; set; }
    public required string InspectionNumber { get; set; }
    public required DateOnly AcceptanceDate { get; set; }
    public required decimal AcceptanceQuantity { get; set; }
    public required decimal AcceptanceAmount { get; set; }
    public decimal TaxAmount { get; set; }
    public string? StorageLocationCode { get; set; }
    public string? Remarks { get; set; }
    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }
}
