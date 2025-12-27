using ProductionManagement.Domain.Models.Cost;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 原価リポジトリインターフェース
/// </summary>
public interface ICostRepository
{
    Task<StandardCost?> FindStandardCostByItemCodeAsync(string itemCode, DateOnly targetDate);

    Task<IReadOnlyList<StandardCost>> FindAllStandardCostsByItemCodeAsync(string itemCode);

    Task<ActualCost?> FindActualCostByWorkOrderNumberAsync(string workOrderNumber);

    Task<CostVariance?> FindCostVarianceByWorkOrderNumberAsync(string workOrderNumber);

    Task<long> SaveStandardCostAsync(StandardCost standardCost);

    Task<long> SaveActualCostAsync(ActualCost actualCost);

    Task<long> SaveCostVarianceAsync(CostVariance variance);

    Task DeleteAllCostVariancesAsync();

    Task DeleteAllActualCostsAsync();

    Task DeleteAllStandardCostsAsync();
}
