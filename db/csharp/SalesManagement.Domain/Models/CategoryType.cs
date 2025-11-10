namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 分類種別マスタ
    /// </summary>
    public class CategoryType
    {
        public string CategoryTypeCode { get; set; } = string.Empty;
        public string CategoryTypeName { get; set; } = string.Empty;
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
