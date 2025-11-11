namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 発注データ
    /// </summary>
    public class PurchaseOrder
    {
        public string PoNo { get; set; } = string.Empty;
        public DateTime PoDate { get; set; }
        public string OrderNo { get; set; } = string.Empty;
        public string SupplierCode { get; set; } = string.Empty;
        public int SupplierBranch { get; set; }
        public string EmployeeCode { get; set; } = string.Empty;
        public DateTime? DueDate { get; set; }
        public string WarehouseCode { get; set; } = string.Empty;
        public int PoAmount { get; set; }
        public int ConsumptionTax { get; set; }
        public string? SlipComment { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
