namespace ProductionManagement.Domain.Models.Inventory;

/// <summary>
/// 棚卸データ
/// </summary>
public class Stocktaking
{
    private string _statusValue = "発行済";

    public long? Id { get; init; }
    public required string StocktakingNumber { get; init; }
    public required string LocationCode { get; init; }
    public required DateOnly StocktakingDate { get; init; }

    /// <summary>
    /// ステータス（データベース格納用の日本語文字列）
    /// </summary>
    public string StatusValue
    {
        get => _statusValue;
        set => _statusValue = value;
    }

    /// <summary>
    /// ステータス（ドメインロジック用の Enum）
    /// </summary>
    public StocktakingStatus Status
    {
        get => StocktakingStatusExtensions.FromDisplayName(_statusValue);
        init => _statusValue = value.ToDisplayName();
    }

    public DateTime CreatedAt { get; init; }
    public DateTime UpdatedAt { get; init; }

    public IReadOnlyList<StocktakingDetail>? Details { get; set; }
}
