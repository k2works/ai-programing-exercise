namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 倉庫マスタ
    /// </summary>
    public class Warehouse
    {
        public string WarehouseCode { get; set; } = string.Empty;
        public string WarehouseName { get; set; } = string.Empty;
        public int WarehouseType { get; set; }
        public string? Address { get; set; }
        public string? PhoneNumber { get; set; }
        public string? ManagerCode { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
