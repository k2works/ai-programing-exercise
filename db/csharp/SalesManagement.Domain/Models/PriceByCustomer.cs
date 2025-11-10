namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 顧客別販売単価のEntityクラス
    /// </summary>
    public class PriceByCustomer
    {
        public string ProductCode { get; set; } = string.Empty;
        public string CustomerCode { get; set; } = string.Empty;
        public int SellingPrice { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
