namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 取引先分類マスタ
    /// </summary>
    public class CompanyCategory
    {
        public string CategoryTypeCode { get; set; } = string.Empty;
        public string CategoryCode { get; set; } = string.Empty;
        public string CategoryName { get; set; } = string.Empty;
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
