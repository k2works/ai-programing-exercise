using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 工程表リポジトリインターフェース
/// </summary>
public interface IRoutingRepository
{
    Task SaveAsync(Routing routing);
    Task<IReadOnlyList<Routing>> FindByItemCodeAsync(string itemCode);
    Task DeleteAllAsync();
}
