using ProductionManagement.Domain.Models.Subcontract;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 消費リポジトリインターフェース
/// </summary>
public interface IConsumptionRepository
{
    Task SaveAsync(Consumption consumption);
    Task<Consumption?> FindByIdAsync(int id);
    Task<Consumption?> FindByConsumptionNumberAsync(string consumptionNumber);
    Task<Consumption?> FindByReceivingNumberAsync(string receivingNumber);
    Task<string?> FindLatestConsumptionNumberAsync(string prefix);
    Task DeleteAllAsync();
}
