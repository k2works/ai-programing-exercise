using ProductionManagement.Domain.Models.Subcontract;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 支給明細リポジトリインターフェース
/// </summary>
public interface ISupplyDetailRepository
{
    Task SaveAsync(SupplyDetail detail);
    Task<IReadOnlyList<SupplyDetail>> FindBySupplyNumberAsync(string supplyNumber);
    Task DeleteAllAsync();
}
