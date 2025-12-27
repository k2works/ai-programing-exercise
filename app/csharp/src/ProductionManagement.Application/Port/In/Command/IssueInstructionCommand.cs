namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 払出指示コマンド
/// </summary>
public class IssueInstructionCommand
{
    public required string OrderNumber { get; init; }

    public required DateOnly InstructionDate { get; init; }

    public required string LocationCode { get; init; }

    public string? Remarks { get; init; }

    public required IReadOnlyList<IssueInstructionDetailCommand> Details { get; init; }
}

/// <summary>
/// 払出指示明細コマンド
/// </summary>
public class IssueInstructionDetailCommand
{
    public required string ItemCode { get; init; }

    public required int RoutingSequence { get; init; }

    public required decimal IssueQuantity { get; init; }
}
