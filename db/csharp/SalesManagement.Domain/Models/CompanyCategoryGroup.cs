namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 取引先分類所属マスタ（多対多の関係）
    /// </summary>
    public class CompanyCategoryGroup
    {
        public string CategoryTypeCode { get; set; } = string.Empty;
        public string CategoryCode { get; set; } = string.Empty;
        public string CompanyCode { get; set; } = string.Empty;
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
