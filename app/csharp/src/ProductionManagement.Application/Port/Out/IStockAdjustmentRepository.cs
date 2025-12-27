using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 在庫調整リポジトリインターフェース
/// </summary>
public interface IStockAdjustmentRepository
{
    Task<IReadOnlyList<StockAdjustment>> FindByStocktakingNumberAsync(string stocktakingNumber);

    Task<long> SaveAsync(StockAdjustment adjustment);

    Task DeleteAllAsync();
}
