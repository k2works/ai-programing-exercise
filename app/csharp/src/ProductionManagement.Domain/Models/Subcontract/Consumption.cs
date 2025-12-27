namespace ProductionManagement.Domain.Models.Subcontract;

/// <summary>
/// 消費データ
/// </summary>
public class Consumption
{
    public int Id { get; set; }
    public required string ConsumptionNumber { get; set; }
    public required string ReceivingNumber { get; set; }
    public required DateOnly ConsumptionDate { get; set; }
    public required string SupplierCode { get; set; }
    public string? Remarks { get; set; }
    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }

    /// <summary>
    /// 消費明細
    /// </summary>
    public IReadOnlyList<ConsumptionDetail> Details { get; set; } = [];
}
