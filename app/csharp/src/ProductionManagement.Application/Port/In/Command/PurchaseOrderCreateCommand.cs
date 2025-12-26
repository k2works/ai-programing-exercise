namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 発注作成コマンド
/// </summary>
public class PurchaseOrderCreateCommand
{
    public required string SupplierCode { get; init; }
    public required DateOnly OrderDate { get; init; }
    public string? OrdererCode { get; init; }
    public string? DepartmentCode { get; init; }
    public decimal? TaxRate { get; init; }
    public string? Remarks { get; init; }
    public required IReadOnlyList<PurchaseOrderDetailCommand> Details { get; init; }
}

/// <summary>
/// 発注明細コマンド
/// </summary>
public class PurchaseOrderDetailCommand
{
    public required string ItemCode { get; init; }
    public required decimal OrderQuantity { get; init; }
    public required DateOnly ExpectedReceivingDate { get; init; }
    public string? OrderNumber { get; init; }
    public string? DeliveryLocationCode { get; init; }
}
