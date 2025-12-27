using ProductionManagement.Domain.Models.Subcontract;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 消費明細リポジトリインターフェース
/// </summary>
public interface IConsumptionDetailRepository
{
    Task SaveAsync(ConsumptionDetail detail);
    Task<IReadOnlyList<ConsumptionDetail>> FindByConsumptionNumberAsync(string consumptionNumber);
    Task<decimal> SumByPurchaseOrderAndItemAsync(string purchaseOrderNumber, int lineNumber, string itemCode);
    Task DeleteAllAsync();
}
