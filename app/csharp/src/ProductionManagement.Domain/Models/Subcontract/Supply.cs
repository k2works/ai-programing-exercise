namespace ProductionManagement.Domain.Models.Subcontract;

/// <summary>
/// 支給データ
/// </summary>
public class Supply
{
    private string _supplyTypeValue = "無償支給";

    public int Id { get; set; }
    public required string SupplyNumber { get; set; }
    public required string PurchaseOrderNumber { get; set; }
    public required int LineNumber { get; set; }
    public required string SupplierCode { get; set; }
    public required DateOnly SupplyDate { get; set; }
    public required string SupplierPersonCode { get; set; }
    public string? Remarks { get; set; }

    /// <summary>
    /// DB から読み取った文字列値（Dapper 用）
    /// </summary>
    public string SupplyTypeValue
    {
        get => _supplyTypeValue;
        set => _supplyTypeValue = value;
    }

    /// <summary>
    /// 支給区分（アプリケーション用）
    /// </summary>
    public SupplyType SupplyType
    {
        get => SupplyTypeExtensions.FromDisplayName(_supplyTypeValue);
        set => _supplyTypeValue = value.GetDisplayName();
    }

    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }

    /// <summary>
    /// 支給明細
    /// </summary>
    public IReadOnlyList<SupplyDetail> Details { get; set; } = [];
}
