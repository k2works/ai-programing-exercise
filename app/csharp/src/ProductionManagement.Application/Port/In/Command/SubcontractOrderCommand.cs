namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 外注発注コマンド
/// </summary>
public class SubcontractOrderCommand
{
    public required string SupplierCode { get; init; }
    public required DateOnly DeliveryDate { get; init; }
    public required string ItemCode { get; init; }
    public required decimal Quantity { get; init; }
    public decimal? UnitPrice { get; init; }
}
