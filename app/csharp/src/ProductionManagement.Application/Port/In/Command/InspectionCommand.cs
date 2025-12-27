using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 受入検査登録コマンド
/// </summary>
public class InspectionCommand
{
    public required string ReceivingNumber { get; init; }
    public required DateOnly InspectionDate { get; init; }
    public required decimal InspectionQuantity { get; init; }
    public required decimal PassedQuantity { get; init; }
    public decimal FailedQuantity { get; init; }
    public InspectionResult InspectionResult { get; init; } = InspectionResult.Passed;
    public string? DefectCode { get; init; }
    public string? InspectorCode { get; init; }
    public string? Remarks { get; init; }
    public string? CreatedBy { get; init; }
}
