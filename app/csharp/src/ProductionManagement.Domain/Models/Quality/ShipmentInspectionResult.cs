namespace ProductionManagement.Domain.Models.Quality;

/// <summary>
/// 出荷検査結果データ
/// </summary>
public class ShipmentInspectionResult
{
    public long? Id { get; init; }
    public required string InspectionNumber { get; init; }
    public required string DefectCode { get; init; }
    public required decimal Quantity { get; init; }
}
