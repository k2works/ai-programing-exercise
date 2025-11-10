namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 取引先グループマスタ
    /// </summary>
    public class CompanyGroup
    {
        public string CompanyGroupCode { get; set; } = string.Empty;
        public string? CompanyGroupName { get; set; }
        public DateTime CreatedAt { get; set; }
        public string? CreatedBy { get; set; }
        public DateTime UpdatedAt { get; set; }
        public string? UpdatedBy { get; set; }
    }
}
