using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 在庫情報リポジトリインターフェース
/// </summary>
public interface IStockRepository
{
    Task<Stock?> FindByLocationAndItemAsync(string locationCode, string itemCode);
    Task<IReadOnlyList<Stock>> FindByLocationAsync(string locationCode);
    Task<IReadOnlyList<Stock>> FindAllAsync();
    Task<long> SaveAsync(Stock stock);
    Task IncreaseByStatusAsync(string locationCode, string itemCode, decimal quantity, StockStatus status);
    Task DecreaseByStatusAsync(string locationCode, string itemCode, decimal quantity, StockStatus status);
    Task ChangeStatusAsync(string locationCode, string itemCode, decimal quantity, StockStatus fromStatus, StockStatus toStatus);
    Task UpdateAsync(Stock stock);
    Task DeleteAllAsync();
}
