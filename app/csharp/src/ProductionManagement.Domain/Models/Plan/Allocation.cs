namespace ProductionManagement.Domain.Models.Plan;

/// <summary>
/// 引当情報
/// </summary>
public class Allocation
{
    private string _allocationTypeValue = "在庫";

    public int Id { get; set; }

    public int RequirementId { get; set; }

    /// <summary>
    /// 引当区分（データベース格納用の日本語文字列）
    /// </summary>
    public string AllocationTypeValue
    {
        get => _allocationTypeValue;
        set => _allocationTypeValue = value;
    }

    /// <summary>
    /// 引当区分（ドメインロジック用の Enum）
    /// </summary>
    public AllocationType AllocationType
    {
        get => AllocationTypeExtensions.FromDisplayName(_allocationTypeValue);
        set => _allocationTypeValue = value.GetDisplayName();
    }

    public int? OrderId { get; set; }

    public DateOnly AllocationDate { get; set; }

    public decimal AllocatedQuantity { get; set; }

    public required string LocationCode { get; set; }

    public DateTime CreatedAt { get; set; }

    public DateTime UpdatedAt { get; set; }

    // リレーション
    public Requirement? Requirement { get; set; }
    public Order? Order { get; set; }
}
