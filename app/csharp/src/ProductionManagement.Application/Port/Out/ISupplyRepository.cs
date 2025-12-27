using ProductionManagement.Domain.Models.Subcontract;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 支給リポジトリインターフェース
/// </summary>
public interface ISupplyRepository
{
    Task SaveAsync(Supply supply);
    Task<Supply?> FindByIdAsync(int id);
    Task<Supply?> FindBySupplyNumberAsync(string supplyNumber);
    Task<IReadOnlyList<Supply>> FindByPurchaseOrderDetailAsync(string purchaseOrderNumber, int lineNumber);
    Task<string?> FindLatestSupplyNumberAsync(string prefix);
    Task DeleteAllAsync();
}
