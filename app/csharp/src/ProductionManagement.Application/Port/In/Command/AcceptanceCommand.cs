namespace ProductionManagement.Application.Port.In.Command;

/// <summary>
/// 検収登録コマンド
/// </summary>
public class AcceptanceCommand
{
    public required string InspectionNumber { get; init; }
    public required DateOnly AcceptanceDate { get; init; }
    public required decimal AcceptanceQuantity { get; init; }
    public required decimal AcceptanceAmount { get; init; }
    public decimal TaxAmount { get; init; }
    public string? StorageLocationCode { get; init; }
    public string? Remarks { get; init; }
    public string? CreatedBy { get; init; }
}
