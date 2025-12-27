using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 完成検査結果リポジトリインターフェース
/// </summary>
public interface ICompletionInspectionResultRepository
{
    Task SaveAsync(CompletionInspectionResult inspectionResult);
    Task<IReadOnlyList<CompletionInspectionResult>> FindByCompletionResultNumberAsync(string completionResultNumber);
    Task DeleteAllAsync();
}
