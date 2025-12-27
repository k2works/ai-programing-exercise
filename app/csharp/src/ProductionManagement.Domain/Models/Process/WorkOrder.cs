namespace ProductionManagement.Domain.Models.Process;

/// <summary>
/// 作業指示データ
/// </summary>
public class WorkOrder
{
    private string _statusValue = "未着手";

    public int Id { get; set; }
    public required string WorkOrderNumber { get; set; }
    public required string OrderNumber { get; set; }
    public required DateOnly WorkOrderDate { get; set; }
    public required string ItemCode { get; set; }
    public required decimal OrderQuantity { get; set; }
    public required string LocationCode { get; set; }
    public required DateOnly PlannedStartDate { get; set; }
    public required DateOnly PlannedEndDate { get; set; }
    public DateOnly? ActualStartDate { get; set; }
    public DateOnly? ActualEndDate { get; set; }
    public decimal CompletedQuantity { get; set; }
    public decimal TotalGoodQuantity { get; set; }
    public decimal TotalDefectQuantity { get; set; }

    public string StatusValue
    {
        get => _statusValue;
        set => _statusValue = value;
    }

    public WorkOrderStatus Status
    {
        get => WorkOrderStatusExtensions.FromDisplayName(_statusValue);
        set => _statusValue = value.GetDisplayName();
    }

    public bool CompletedFlag { get; set; }
    public string? Remarks { get; set; }
    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }

    public IReadOnlyList<WorkOrderDetail> Details { get; set; } = [];
}
