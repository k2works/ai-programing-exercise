namespace ProductionManagement.Domain.Models.Purchase;

/// <summary>
/// 発注データ
/// </summary>
public class PurchaseOrder
{
    private string _statusValue = "作成中";

    public int Id { get; set; }
    public required string PurchaseOrderNumber { get; set; }
    public required DateOnly OrderDate { get; set; }
    public required string SupplierCode { get; set; }
    public string? OrdererCode { get; set; }
    public string? DepartmentCode { get; set; }

    /// <summary>
    /// DB から読み取った文字列値（Dapper 用）
    /// </summary>
    public string StatusValue
    {
        get => _statusValue;
        set => _statusValue = value;
    }

    /// <summary>
    /// ステータス（アプリケーション用）
    /// </summary>
    public PurchaseOrderStatus Status
    {
        get => PurchaseOrderStatusExtensions.FromDisplayName(_statusValue);
        set => _statusValue = value.GetDisplayName();
    }

    public string? Remarks { get; set; }
    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }

    // リレーション
    public IReadOnlyList<PurchaseOrderDetail> Details { get; set; } = [];
}
