namespace ProductionManagement.Domain.Models.Quality;

/// <summary>
/// ロットマスタ
/// </summary>
public class LotMaster
{
    private string _lotTypeValue = "製造ロット";

    public long? Id { get; init; }
    public required string LotNumber { get; init; }
    public required string ItemCode { get; init; }

    /// <summary>
    /// ロット種別（データベース格納用の日本語文字列）
    /// </summary>
    public string LotTypeValue
    {
        get => _lotTypeValue;
        set => _lotTypeValue = value;
    }

    /// <summary>
    /// ロット種別（ドメインロジック用の Enum）
    /// </summary>
    public LotType LotType
    {
        get => LotTypeExtensions.FromDisplayName(_lotTypeValue);
        init => _lotTypeValue = value.ToDisplayName();
    }

    public DateOnly? ManufactureDate { get; init; }
    public DateOnly? ExpirationDate { get; init; }
    public required decimal Quantity { get; init; }
    public DateTime CreatedAt { get; init; }
    public DateTime UpdatedAt { get; init; }

    public IReadOnlyList<LotComposition>? ParentLotRelations { get; set; }
    public IReadOnlyList<LotComposition>? ChildLotRelations { get; set; }
}
