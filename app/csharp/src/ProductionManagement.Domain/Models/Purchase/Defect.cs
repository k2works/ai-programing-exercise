namespace ProductionManagement.Domain.Models.Purchase;

/// <summary>
/// 欠点マスタ
/// </summary>
public class Defect
{
    public int Id { get; set; }
    public required string DefectCode { get; set; }
    public required string DefectName { get; set; }
    public string? DefectCategory { get; set; }
    public int DisplayOrder { get; set; }
    public bool IsActive { get; set; } = true;
    public DateTime? CreatedAt { get; set; }
    public string? CreatedBy { get; set; }
    public DateTime? UpdatedAt { get; set; }
    public string? UpdatedBy { get; set; }
}
