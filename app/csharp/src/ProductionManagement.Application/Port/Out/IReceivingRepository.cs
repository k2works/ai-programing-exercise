using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 入荷受入リポジトリインターフェース
/// </summary>
public interface IReceivingRepository
{
    Task SaveAsync(Receiving receiving);
    Task<Receiving?> FindByIdAsync(int id);
    Task<Receiving?> FindByReceivingNumberAsync(string receivingNumber);
    Task<IReadOnlyList<Receiving>> FindByPurchaseOrderNumberAsync(string purchaseOrderNumber);
    Task<string?> FindLatestReceivingNumberAsync(string prefix);
    Task DeleteAllAsync();
}
