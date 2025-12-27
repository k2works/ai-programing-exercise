using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 在庫増減コマンド
/// </summary>
public class StockChangeCommand
{
    public required string LocationCode { get; init; }
    public required string ItemCode { get; init; }
    public required decimal Quantity { get; init; }
    public required StockStatus StockStatus { get; init; }
}
