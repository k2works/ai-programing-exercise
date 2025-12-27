namespace ProductionManagement.Domain.Models.Purchase;

/// <summary>
/// 入荷受入データ
/// </summary>
public class Receiving
{
    private string _receivingTypeValue = "通常入荷";

    public int Id { get; set; }
    public required string ReceivingNumber { get; set; }
    public required string PurchaseOrderNumber { get; set; }
    public required int LineNumber { get; set; }
    public required DateOnly ReceivingDate { get; set; }
    public required string ItemCode { get; set; }
    public required decimal ReceivingQuantity { get; set; }
    public string? DeliveryNoteNumber { get; set; }
    public string? LotNumber { get; set; }
    public string? ReceivingLocationCode { get; set; }
    public string? Remarks { get; set; }

    /// <summary>
    /// DB から読み取った文字列値（Dapper 用）
    /// </summary>
    public string ReceivingTypeValue
    {
        get => _receivingTypeValue;
        set => _receivingTypeValue = value;
    }

    /// <summary>
    /// 入荷区分（アプリケーション用）
    /// </summary>
    public ReceivingType ReceivingType
    {
        get => ReceivingTypeExtensions.FromDisplayName(_receivingTypeValue);
        set => _receivingTypeValue = value.GetDisplayName();
    }

    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }
}
