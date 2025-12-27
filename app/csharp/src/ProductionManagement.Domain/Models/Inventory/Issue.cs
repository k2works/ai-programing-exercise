namespace ProductionManagement.Domain.Models.Inventory;

/// <summary>
/// 払出
/// </summary>
public class Issue
{
    public long? Id { get; init; }

    public required string IssueNumber { get; init; }

    public required string WorkOrderNumber { get; init; }

    public required int RoutingSequence { get; init; }

    public required string LocationCode { get; init; }

    public required DateOnly IssueDate { get; init; }

    public required string IssuerCode { get; init; }

    public DateTime CreatedAt { get; init; }

    public DateTime UpdatedAt { get; init; }

    public IReadOnlyList<IssueDetail>? Details { get; set; }
}
