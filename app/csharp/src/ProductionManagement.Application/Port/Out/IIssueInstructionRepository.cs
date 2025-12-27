using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 払出指示リポジトリインターフェース
/// </summary>
public interface IIssueInstructionRepository
{
    Task<IssueInstruction?> FindByInstructionNumberAsync(string instructionNumber);

    Task<IReadOnlyList<IssueInstructionDetail>> FindDetailsByInstructionNumberAsync(string instructionNumber);

    Task<string?> FindLatestInstructionNumberAsync(string pattern);

    Task<long> SaveAsync(IssueInstruction instruction);

    Task SaveDetailAsync(IssueInstructionDetail detail);

    Task DeleteAllAsync();

    Task DeleteAllDetailsAsync();
}
