namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 商品分類マスタのEntityクラス
    /// </summary>
    public class ProductCategory
    {
        public string ProductCategoryCode { get; set; } = string.Empty;
        public string ProductCategoryName { get; set; } = string.Empty;
        public int ProductCategoryLevel { get; set; }
        public string ProductCategoryPath { get; set; } = string.Empty;
        public int LowestLevelFlag { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
