namespace ProductionManagement.Domain.Models.Purchase;

/// <summary>
/// 諸口品目情報（マスタに登録されていない臨時品目）
/// </summary>
public class MiscellaneousItem
{
    public int Id { get; set; }
    public required string PurchaseOrderNumber { get; set; }
    public required int LineNumber { get; set; }
    public required string ItemCode { get; set; }
    public required string ItemName { get; set; }
    public string? Specification { get; set; }
    public string? DrawingNumberMaker { get; set; }
    public string? Version { get; set; }
    public DateTime? CreatedAt { get; set; }
    public DateTime? UpdatedAt { get; set; }
}
