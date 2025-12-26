using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 所要情報リポジトリ（Output Port）
/// </summary>
public interface IRequirementRepository
{
    Task SaveAsync(Requirement requirement);
    Task<Requirement?> FindByIdAsync(int id);
    Task<IReadOnlyList<Requirement>> FindByOrderIdAsync(int orderId);
    Task UpdateAllocationAsync(int id, decimal allocatedQuantity, decimal shortageQuantity);
    Task DeleteAllAsync();
}
