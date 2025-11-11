namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 売上データ
    /// </summary>
    public class Sales
    {
        public string SalesNo { get; set; } = string.Empty;
        public DateTime SalesDate { get; set; }
        public string OrderNo { get; set; } = string.Empty;
        public string DepartmentCode { get; set; } = string.Empty;
        public DateTime StartDate { get; set; }
        public string CompanyCode { get; set; } = string.Empty;
        public string EmployeeCode { get; set; } = string.Empty;
        public string WarehouseCode { get; set; } = string.Empty;
        public int SalesAmount { get; set; }
        public int ConsumptionTax { get; set; }
        public string? SlipComment { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
