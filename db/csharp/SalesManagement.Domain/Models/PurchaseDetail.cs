namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 仕入データ明細
    /// </summary>
    public class PurchaseDetail
    {
        public string PurchaseNo { get; set; } = string.Empty;
        public int PurchaseRowNo { get; set; }
        public string ProductCode { get; set; } = string.Empty;
        public string ProductName { get; set; } = string.Empty;
        public int UnitPrice { get; set; }
        public int Quantity { get; set; }
        public int ConsumptionTaxRate { get; set; }
        public string LotNo { get; set; } = string.Empty;
        public string WarehouseCode { get; set; } = string.Empty;
        public int Discount { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
