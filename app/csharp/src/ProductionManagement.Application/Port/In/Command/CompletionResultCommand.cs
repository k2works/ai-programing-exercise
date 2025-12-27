namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 完成実績報告コマンド
/// </summary>
public class CompletionResultCommand
{
    public required string WorkOrderNumber { get; init; }
    public required DateOnly CompletionDate { get; init; }
    public required decimal CompletedQuantity { get; init; }
    public required decimal GoodQuantity { get; init; }
    public required decimal DefectQuantity { get; init; }
    public string? Remarks { get; init; }
    public IReadOnlyList<CompletionInspectionResultCommand>? InspectionResults { get; init; }
}

/// <summary>
/// 完成検査結果コマンド
/// </summary>
public class CompletionInspectionResultCommand
{
    public required string DefectCode { get; init; }
    public required decimal Quantity { get; init; }
}
