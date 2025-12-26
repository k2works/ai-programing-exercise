using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 引当情報リポジトリ（Output Port）
/// </summary>
public interface IAllocationRepository
{
    Task SaveAsync(Allocation allocation);
    Task<IReadOnlyList<Allocation>> FindByRequirementIdAsync(int requirementId);
    Task DeleteAllAsync();
}
