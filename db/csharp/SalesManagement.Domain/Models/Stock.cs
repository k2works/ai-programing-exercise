namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 在庫データ（5フィールド複合主キー）
    /// </summary>
    public class Stock
    {
        public string WarehouseCode { get; set; } = string.Empty;
        public string ProductCode { get; set; } = string.Empty;
        public string LotNo { get; set; } = string.Empty;
        public string StockType { get; set; } = "1";
        public string QualityType { get; set; } = "G";
        public int ActualQuantity { get; set; }
        public int ValidQuantity { get; set; }
        public DateTime? LastDeliveryDate { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
