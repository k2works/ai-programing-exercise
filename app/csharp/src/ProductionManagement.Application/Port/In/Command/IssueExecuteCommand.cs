namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 払出実行コマンド
/// </summary>
public class IssueExecuteCommand
{
    public required string WorkOrderNumber { get; init; }

    public required int RoutingSequence { get; init; }

    public required DateOnly IssueDate { get; init; }

    public required string IssuerCode { get; init; }

    public string? LocationCode { get; init; }

    public required IReadOnlyList<IssueDetailCommand> Details { get; init; }
}

/// <summary>
/// 払出明細コマンド
/// </summary>
public class IssueDetailCommand
{
    public required string ItemCode { get; init; }

    public required decimal IssueQuantity { get; init; }
}
