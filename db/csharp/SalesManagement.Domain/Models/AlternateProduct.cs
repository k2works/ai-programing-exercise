namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 代替商品のEntityクラス
    /// </summary>
    public class AlternateProduct
    {
        public string ProductCode { get; set; } = string.Empty;
        public string AlternateProductCode { get; set; } = string.Empty;
        public int Priority { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
