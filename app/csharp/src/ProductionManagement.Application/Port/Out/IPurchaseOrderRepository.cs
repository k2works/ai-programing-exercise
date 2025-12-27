using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 発注リポジトリインターフェース
/// </summary>
public interface IPurchaseOrderRepository
{
    Task SaveAsync(PurchaseOrder purchaseOrder);
    Task<PurchaseOrder?> FindByIdAsync(int id);
    Task<PurchaseOrder?> FindByPurchaseOrderNumberAsync(string purchaseOrderNumber);
    Task<IReadOnlyList<PurchaseOrder>> FindAllAsync();
    Task<string?> FindLatestPurchaseOrderNumberAsync(string prefix);
    Task UpdateStatusAsync(string purchaseOrderNumber, PurchaseOrderStatus status);
    Task DeleteAllAsync();
}
