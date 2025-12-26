namespace ProductionManagement.Domain.Models.Plan;

/// <summary>
/// 所要情報
/// </summary>
public class Requirement
{
    public int Id { get; set; }

    public required string RequirementNumber { get; set; }

    public int OrderId { get; set; }

    public required string ItemCode { get; set; }

    public DateOnly DueDate { get; set; }

    public decimal RequiredQuantity { get; set; }

    public decimal AllocatedQuantity { get; set; } = 0m;

    public decimal ShortageQuantity { get; set; } = 0m;

    public required string LocationCode { get; set; }

    public DateTime CreatedAt { get; set; }

    public DateTime UpdatedAt { get; set; }

    // リレーション
    public Order? Order { get; set; }
    public Item.Item? Item { get; set; }
    public IList<Allocation>? Allocations { get; set; }
}
