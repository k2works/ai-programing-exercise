namespace ProductionManagement.Domain.Models.Plan;

/// <summary>
/// オーダ情報（購買・製造共通）
/// </summary>
public class Order
{
    private string _orderTypeValue = "製造";
    private string _statusValue = "草案";

    public int Id { get; set; }

    public required string OrderNumber { get; set; }

    /// <summary>
    /// オーダ種別（データベース格納用の日本語文字列）
    /// </summary>
    public string OrderTypeValue
    {
        get => _orderTypeValue;
        set => _orderTypeValue = value;
    }

    /// <summary>
    /// オーダ種別（ドメインロジック用の Enum）
    /// </summary>
    public OrderType OrderType
    {
        get => OrderTypeExtensions.FromDisplayName(_orderTypeValue);
        set => _orderTypeValue = value.GetDisplayName();
    }

    public required string ItemCode { get; set; }

    public DateOnly StartDate { get; set; }

    public DateOnly DueDate { get; set; }

    public DateOnly? ExpirationDate { get; set; }

    public decimal PlanQuantity { get; set; }

    public required string LocationCode { get; set; }

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
    public PlanStatus Status
    {
        get => PlanStatusExtensions.FromDisplayName(_statusValue);
        set => _statusValue = value.GetDisplayName();
    }

    public int? MpsId { get; set; }

    public int? ParentOrderId { get; set; }

    public DateTime CreatedAt { get; set; }

    public string? CreatedBy { get; set; }

    public DateTime UpdatedAt { get; set; }

    public string? UpdatedBy { get; set; }

    // リレーション
    public Item.Item? Item { get; set; }
    public MasterProductionSchedule? Mps { get; set; }
    public Order? ParentOrder { get; set; }
    public IList<Order>? ChildOrders { get; set; }
    public IList<Requirement>? Requirements { get; set; }
}
