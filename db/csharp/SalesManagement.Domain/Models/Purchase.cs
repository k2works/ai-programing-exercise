namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 仕入データ
    /// </summary>
    public class Purchase
    {
        public string PurchaseNo { get; set; } = string.Empty;
        public DateTime PurchaseDate { get; set; }
        public string PoNo { get; set; } = string.Empty;
        public string SupplierCode { get; set; } = string.Empty;
        public int SupplierBranch { get; set; }
        public string EmployeeCode { get; set; } = string.Empty;
        public string WarehouseCode { get; set; } = string.Empty;
        public int PurchaseAmount { get; set; }
        public int ConsumptionTax { get; set; }
        public string? SlipComment { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
