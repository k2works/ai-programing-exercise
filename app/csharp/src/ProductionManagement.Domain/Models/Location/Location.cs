namespace ProductionManagement.Domain.Models.Location;

/// <summary>
/// 場所マスタ
/// </summary>
public class Location
{
    private string _locationTypeValue = "倉庫";

    public required string LocationCode { get; set; }

    public required string LocationName { get; set; }

    /// <summary>
    /// 場所区分（データベース格納用の日本語文字列）
    /// </summary>
    public string LocationTypeValue
    {
        get => _locationTypeValue;
        set => _locationTypeValue = value;
    }

    /// <summary>
    /// 場所区分（ドメインロジック用の Enum）
    /// </summary>
    public LocationType LocationType
    {
        get => LocationTypeExtensions.FromDisplayName(_locationTypeValue);
        set => _locationTypeValue = value.GetDisplayName();
    }

    public string? ParentLocationCode { get; set; }

    public DateTime CreatedAt { get; set; }

    public DateTime UpdatedAt { get; set; }
}
