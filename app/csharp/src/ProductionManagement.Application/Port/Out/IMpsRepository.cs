using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 基準生産計画リポジトリ（Output Port）
/// </summary>
public interface IMpsRepository
{
    Task SaveAsync(MasterProductionSchedule mps);
    Task<MasterProductionSchedule?> FindByIdAsync(int id);
    Task<MasterProductionSchedule?> FindByMpsNumberAsync(string mpsNumber);
    Task<IReadOnlyList<MasterProductionSchedule>> FindByStatusAsync(PlanStatus status);
    Task UpdateStatusAsync(int id, PlanStatus status);
    Task DeleteAllAsync();
}
