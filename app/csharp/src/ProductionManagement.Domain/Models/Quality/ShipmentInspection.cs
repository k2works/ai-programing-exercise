namespace ProductionManagement.Domain.Models.Quality;

/// <summary>
/// 出荷検査データ
/// </summary>
public class ShipmentInspection
{
    private string _judgmentValue = "合格";

    public long? Id { get; init; }
    public required string InspectionNumber { get; init; }
    public required string ShipmentNumber { get; init; }
    public required string ItemCode { get; init; }
    public required DateOnly InspectionDate { get; init; }
    public required string InspectorCode { get; init; }
    public required decimal InspectionQuantity { get; init; }
    public required decimal PassedQuantity { get; init; }
    public required decimal FailedQuantity { get; init; }

    /// <summary>
    /// 判定（データベース格納用の日本語文字列）
    /// </summary>
    public string JudgmentValue
    {
        get => _judgmentValue;
        set => _judgmentValue = value;
    }

    /// <summary>
    /// 判定（ドメインロジック用の Enum）
    /// </summary>
    public InspectionJudgment Judgment
    {
        get => InspectionJudgmentExtensions.FromDisplayName(_judgmentValue);
        init => _judgmentValue = value.ToDisplayName();
    }

    public DateTime CreatedAt { get; init; }
    public DateTime UpdatedAt { get; init; }

    public IReadOnlyList<ShipmentInspectionResult>? Results { get; set; }
}
