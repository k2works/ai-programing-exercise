namespace ProductionManagement.Domain.Models.Purchase;

/// <summary>
/// 受入検査データ
/// </summary>
public class Inspection
{
    private string _inspectionResultValue = "合格";

    public int Id { get; set; }
    public required string InspectionNumber { get; set; }
    public required string ReceivingNumber { get; set; }
    public required DateOnly InspectionDate { get; set; }
    public required decimal InspectionQuantity { get; set; }
    public decimal PassedQuantity { get; set; }
    public decimal FailedQuantity { get; set; }
    public string? DefectCode { get; set; }
    public string? InspectorCode { get; set; }
    public string? Remarks { get; set; }

    /// <summary>
    /// DB から読み取った文字列値（Dapper 用）
    /// </summary>
    public string InspectionResultValue
    {
        get => _inspectionResultValue;
        set => _inspectionResultValue = value;
    }

    /// <summary>
    /// 検査結果（アプリケーション用）
    /// </summary>
    public InspectionResult InspectionResult
    {
        get => InspectionResultExtensions.FromDisplayName(_inspectionResultValue);
        set => _inspectionResultValue = value.GetDisplayName();
    }

    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }
}
