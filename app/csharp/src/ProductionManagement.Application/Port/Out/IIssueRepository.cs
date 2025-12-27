using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 払出リポジトリインターフェース
/// </summary>
public interface IIssueRepository
{
    Task<Issue?> FindByIssueNumberAsync(string issueNumber);

    Task<IReadOnlyList<IssueDetail>> FindDetailsByIssueNumberAsync(string issueNumber);

    Task<IReadOnlyList<Issue>> FindByWorkOrderNumberAsync(string workOrderNumber);

    Task<string?> FindLatestIssueNumberAsync(string pattern);

    Task<long> SaveAsync(Issue issue);

    Task SaveDetailAsync(IssueDetail detail);

    Task DeleteAllAsync();

    Task DeleteAllDetailsAsync();
}
