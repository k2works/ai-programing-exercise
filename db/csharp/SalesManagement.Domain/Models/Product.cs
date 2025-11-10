namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 商品マスタのEntityクラス
    /// </summary>
    public class Product
    {
        public string ProductCode { get; set; } = string.Empty;
        public string ProductFormalName { get; set; } = string.Empty;
        public string ProductAbbreviation { get; set; } = string.Empty;
        public string ProductNameKana { get; set; } = string.Empty;
        public string ProductType { get; set; } = string.Empty;
        public string? ModelNumber { get; set; }
        public int SellingPrice { get; set; }
        public int PurchasePrice { get; set; }
        public int CostOfSales { get; set; }
        public int TaxType { get; set; }
        public string ProductCategoryCode { get; set; } = string.Empty;
        public int MiscellaneousType { get; set; }
        public int InventoryManagementFlag { get; set; }
        public int InventoryAllocationFlag { get; set; }
        public string? SupplierCode { get; set; }
        public int? SupplierBranch { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
