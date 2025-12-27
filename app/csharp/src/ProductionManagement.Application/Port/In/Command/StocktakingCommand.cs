namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 棚卸表発行コマンド
/// </summary>
public class StocktakingIssueCommand
{
    public required string LocationCode { get; init; }
    public required DateOnly StocktakingDate { get; init; }
}

/// <summary>
/// 実棚入力明細
/// </summary>
public class ActualCountDetailInput
{
    public required string ItemCode { get; init; }
    public required decimal ActualQuantity { get; init; }
}

/// <summary>
/// 実棚数量入力コマンド
/// </summary>
public class ActualCountInputCommand
{
    public required string StocktakingNumber { get; init; }
    public required IReadOnlyList<ActualCountDetailInput> Details { get; init; }
}

/// <summary>
/// 棚卸確定コマンド
/// </summary>
public class StocktakingConfirmCommand
{
    public required string StocktakingNumber { get; init; }
    public required string AdjusterCode { get; init; }
    public required string AdjustmentReasonCode { get; init; }
}
