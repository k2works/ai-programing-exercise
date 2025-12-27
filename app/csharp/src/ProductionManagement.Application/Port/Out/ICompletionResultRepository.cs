using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 完成実績リポジトリインターフェース
/// </summary>
public interface ICompletionResultRepository
{
    Task SaveAsync(CompletionResult completionResult);
    Task<CompletionResult?> FindByCompletionResultNumberAsync(string completionResultNumber);
    Task<IReadOnlyList<CompletionResult>> FindByWorkOrderNumberAsync(string workOrderNumber);
    Task<string?> FindLatestCompletionResultNumberAsync(string prefix);
    Task DeleteAllAsync();
}
