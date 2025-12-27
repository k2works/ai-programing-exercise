namespace ProductionManagement.Domain.Models.Inventory;

/// <summary>
/// 払出明細
/// </summary>
public class IssueDetail
{
    public long? Id { get; init; }

    public required string IssueNumber { get; init; }

    public required int LineNumber { get; init; }

    public required string ItemCode { get; init; }

    public required decimal IssueQuantity { get; init; }

    public DateTime CreatedAt { get; init; }

    public DateTime UpdatedAt { get; init; }
}
