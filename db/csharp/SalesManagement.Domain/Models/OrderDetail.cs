namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 受注データ明細
    /// </summary>
    public class OrderDetail
    {
        public string OrderNo { get; set; } = string.Empty;
        public int OrderRowNo { get; set; }
        public string ProductCode { get; set; } = string.Empty;
        public string ProductName { get; set; } = string.Empty;
        public int UnitPrice { get; set; }
        public int Quantity { get; set; }
        public int ConsumptionTaxRate { get; set; }
        public int ReserveQuantity { get; set; }
        public int DeliveryOrderQuantity { get; set; }
        public int DeliveredQuantity { get; set; }
        public int CompleteFlag { get; set; }
        public int Discount { get; set; }
        public DateTime? DeliveryDate { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
