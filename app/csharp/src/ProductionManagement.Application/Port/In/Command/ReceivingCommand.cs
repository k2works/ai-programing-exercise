using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 入荷登録コマンド
/// </summary>
public class ReceivingCommand
{
    public required string PurchaseOrderNumber { get; init; }
    public required int LineNumber { get; init; }
    public required DateOnly ReceivingDate { get; init; }
    public required decimal ReceivingQuantity { get; init; }
    public ReceivingType ReceivingType { get; init; } = ReceivingType.Normal;
    public string? DeliveryNoteNumber { get; init; }
    public string? LotNumber { get; init; }
    public string? ReceivingLocationCode { get; init; }
    public string? Remarks { get; init; }
    public string? CreatedBy { get; init; }
}
