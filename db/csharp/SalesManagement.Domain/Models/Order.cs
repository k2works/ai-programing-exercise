namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 受注データ
    /// </summary>
    public class Order
    {
        public string OrderNo { get; set; } = string.Empty;
        public DateTime OrderDate { get; set; }
        public string DepartmentCode { get; set; } = string.Empty;
        public DateTime StartDate { get; set; }
        public string CustomerCode { get; set; } = string.Empty;
        public int? CustomerBranch { get; set; }
        public string EmployeeCode { get; set; } = string.Empty;
        public DateTime? RequiredDate { get; set; }
        public string? CustomerOrderNo { get; set; }
        public string WarehouseCode { get; set; } = string.Empty;
        public int OrderAmount { get; set; }
        public int ConsumptionTax { get; set; }
        public string? SlipComment { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
