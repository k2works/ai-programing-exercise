namespace ProductionManagement.Domain.Models.Quality;

/// <summary>
/// ロット構成
/// </summary>
public class LotComposition
{
    public long? Id { get; init; }
    public required string ParentLotNumber { get; init; }
    public required string ChildLotNumber { get; init; }
    public required decimal UsedQuantity { get; init; }
}
