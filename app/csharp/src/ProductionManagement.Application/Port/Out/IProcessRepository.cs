using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 工程マスタリポジトリインターフェース
/// </summary>
public interface IProcessRepository
{
    Task SaveAsync(Process process);
    Task<Process?> FindByProcessCodeAsync(string processCode);
    Task DeleteAllAsync();
}
