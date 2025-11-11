namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 発注データ明細
    /// </summary>
    public class PurchaseOrderDetail
    {
        public string PoNo { get; set; } = string.Empty;
        public int PoRowNo { get; set; }
        public string ProductCode { get; set; } = string.Empty;
        public string ProductName { get; set; } = string.Empty;
        public int UnitPrice { get; set; }
        public int Quantity { get; set; }
        public int ConsumptionTaxRate { get; set; }
        public int ReceivedQuantity { get; set; }
        public int Discount { get; set; }
        public DateTime? DueDate { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
