using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 検収リポジトリインターフェース
/// </summary>
public interface IAcceptanceRepository
{
    Task SaveAsync(Acceptance acceptance);
    Task<Acceptance?> FindByIdAsync(int id);
    Task<Acceptance?> FindByAcceptanceNumberAsync(string acceptanceNumber);
    Task<IReadOnlyList<Acceptance>> FindByInspectionNumberAsync(string inspectionNumber);
    Task<string?> FindLatestAcceptanceNumberAsync(string prefix);
    Task DeleteAllAsync();
}
