namespace ProductionManagement.Domain.Models.Quality;

/// <summary>
/// 欠点マスタ
/// </summary>
public class Defect
{
    public required string DefectCode { get; init; }
    public required string DefectName { get; init; }
    public string? DefectCategory { get; init; }
    public DateTime CreatedAt { get; init; }
    public DateTime UpdatedAt { get; init; }
}
