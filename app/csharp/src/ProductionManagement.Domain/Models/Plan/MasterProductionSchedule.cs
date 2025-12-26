namespace ProductionManagement.Domain.Models.Plan;

/// <summary>
/// 基準生産計画（MPS）
/// </summary>
public class MasterProductionSchedule
{
    private string _statusValue = "草案";

    public int Id { get; set; }

    public required string MpsNumber { get; set; }

    public DateOnly PlanDate { get; set; }

    public required string ItemCode { get; set; }

    public decimal PlanQuantity { get; set; }

    public DateOnly DueDate { get; set; }

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

    public string? LocationCode { get; set; }

    public string? Remarks { get; set; }

    public DateTime CreatedAt { get; set; }

    public string? CreatedBy { get; set; }

    public DateTime UpdatedAt { get; set; }

    public string? UpdatedBy { get; set; }

    // リレーション
    public Item.Item? Item { get; set; }
}
