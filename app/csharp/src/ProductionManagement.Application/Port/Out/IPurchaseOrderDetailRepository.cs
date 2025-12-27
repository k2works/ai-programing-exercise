using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 発注明細リポジトリインターフェース
/// </summary>
public interface IPurchaseOrderDetailRepository
{
    Task SaveAsync(PurchaseOrderDetail detail);
    Task<IReadOnlyList<PurchaseOrderDetail>> FindByPurchaseOrderNumberAsync(string purchaseOrderNumber);
    Task<PurchaseOrderDetail?> FindByPurchaseOrderNumberAndLineNumberAsync(
        string purchaseOrderNumber, int lineNumber);
    Task UpdateReceivedQuantityAsync(string purchaseOrderNumber, int lineNumber, decimal receivedQuantity);
    Task UpdateInspectedQuantityAsync(string purchaseOrderNumber, int lineNumber, decimal inspectedQuantity);
    Task UpdateAcceptedQuantityAsync(string purchaseOrderNumber, int lineNumber, decimal acceptedQuantity);
    Task DeleteAllAsync();
}
