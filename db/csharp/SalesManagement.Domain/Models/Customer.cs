namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 顧客マスタ（取引先の顧客としての役割）
    /// </summary>
    public class Customer
    {
        public string CustomerCode { get; set; } = string.Empty;
        public int CustomerBranch { get; set; }
        public int CustomerType { get; set; }
        public string ArCode { get; set; } = string.Empty;
        public int? ArBranch { get; set; }
        public string PayerCode { get; set; } = string.Empty;
        public int? PayerBranch { get; set; }
        public string CustomerName { get; set; } = string.Empty;
        public string? CustomerNameKana { get; set; }
        public string EmployeeCode { get; set; } = string.Empty;
        public string? CustomerUserName { get; set; }
        public string? CustomerDepartmentName { get; set; }
        public string? CustomerZipCode { get; set; }
        public string? CustomerState { get; set; }
        public string? CustomerAddress1 { get; set; }
        public string? CustomerAddress2 { get; set; }
        public string? CustomerTel { get; set; }
        public string? CustomerFax { get; set; }
        public string? CustomerEmail { get; set; }
        public int CustomerArType { get; set; }
        public int CustomerCloseDate1 { get; set; }
        public int CustomerPayMonths1 { get; set; }
        public int? CustomerPayDates1 { get; set; }
        public int CustomerPayMethod1 { get; set; }
        public int CustomerCloseDate2 { get; set; }
        public int CustomerPayMonths2 { get; set; }
        public int? CustomerPayDates2 { get; set; }
        public int CustomerPayMethod2 { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
