using ProductionManagement.Domain.Models.Subcontract;

namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 支給作成コマンド
/// </summary>
public class SupplyCommand
{
    public required string PurchaseOrderNumber { get; init; }
    public required int LineNumber { get; init; }
    public required string SupplierCode { get; init; }
    public required DateOnly SupplyDate { get; init; }
    public required string SupplierPersonCode { get; init; }
    public SupplyType SupplyType { get; init; } = SupplyType.Free;
    public string? Remarks { get; init; }
    public string? CreatedBy { get; init; }
    public required IReadOnlyList<SupplyDetailCommand> Details { get; init; }
}

/// <summary>
/// 支給明細コマンド
/// </summary>
public class SupplyDetailCommand
{
    public required string ItemCode { get; init; }
    public required decimal Quantity { get; init; }
    public required decimal UnitPrice { get; init; }
    public string? Remarks { get; init; }
}
