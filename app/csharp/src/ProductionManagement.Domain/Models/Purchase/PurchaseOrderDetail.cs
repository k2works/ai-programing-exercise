namespace ProductionManagement.Domain.Models.Purchase;

/// <summary>
/// 発注明細データ
/// </summary>
public class PurchaseOrderDetail
{
    public int Id { get; set; }
    public required string PurchaseOrderNumber { get; set; }
    public required int LineNumber { get; set; }
    public string? OrderNumber { get; set; }
    public string? DeliveryLocationCode { get; set; }
    public required string ItemCode { get; set; }
    public bool MiscellaneousItemFlag { get; set; }
    public required DateOnly ExpectedReceivingDate { get; set; }
    public DateOnly? ConfirmedDeliveryDate { get; set; }
    public required decimal OrderUnitPrice { get; set; }
    public required decimal OrderQuantity { get; set; }
    public decimal ReceivedQuantity { get; set; }
    public decimal InspectedQuantity { get; set; }
    public decimal AcceptedQuantity { get; set; }
    public required decimal OrderAmount { get; set; }
    public decimal TaxAmount { get; set; }
    public bool CompletedFlag { get; set; }
    public string? DetailRemarks { get; set; }
    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }
}
