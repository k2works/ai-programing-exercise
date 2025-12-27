using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 在庫状態変更コマンド
/// </summary>
public class StockStatusChangeCommand
{
    public required string LocationCode { get; init; }
    public required string ItemCode { get; init; }
    public required decimal Quantity { get; init; }
    public required StockStatus FromStatus { get; init; }
    public required StockStatus ToStatus { get; init; }
}
