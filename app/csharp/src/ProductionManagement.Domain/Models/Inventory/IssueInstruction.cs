namespace ProductionManagement.Domain.Models.Inventory;

/// <summary>
/// 払出指示
/// </summary>
public class IssueInstruction
{
    public long? Id { get; init; }

    public required string InstructionNumber { get; init; }

    public required string OrderNumber { get; init; }

    public required DateOnly InstructionDate { get; init; }

    public required string LocationCode { get; init; }

    public string? Remarks { get; init; }

    public DateTime CreatedAt { get; init; }

    public DateTime UpdatedAt { get; init; }

    public IReadOnlyList<IssueInstructionDetail>? Details { get; set; }
}
