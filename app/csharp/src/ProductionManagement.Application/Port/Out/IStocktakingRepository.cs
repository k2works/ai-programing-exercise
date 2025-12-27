using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 棚卸リポジトリインターフェース
/// </summary>
public interface IStocktakingRepository
{
    Task<Stocktaking?> FindByStocktakingNumberAsync(string stocktakingNumber);

    Task<IReadOnlyList<StocktakingDetail>> FindDetailsByStocktakingNumberAsync(string stocktakingNumber);

    Task<string?> FindLatestStocktakingNumberAsync(string pattern);

    Task<long> SaveAsync(Stocktaking stocktaking);

    Task SaveDetailAsync(StocktakingDetail detail);

    Task UpdateDetailAsync(long id, decimal actualQuantity, decimal differenceQuantity);

    Task UpdateStatusAsync(string stocktakingNumber, StocktakingStatus status);

    Task DeleteAllAsync();

    Task DeleteAllDetailsAsync();
}
