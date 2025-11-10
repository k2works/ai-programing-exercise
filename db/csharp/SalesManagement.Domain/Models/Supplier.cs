namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 仕入先マスタ（取引先の仕入先としての役割）
    /// </summary>
    public class Supplier
    {
        public string SupplierCode { get; set; } = string.Empty;
        public int SupplierBranch { get; set; }
        public string SupplierName { get; set; } = string.Empty;
        public string? SupplierNameKana { get; set; }
        public string? SupplierUserName { get; set; }
        public string? SupplierDepartmentName { get; set; }
        public string? SupplierZipCode { get; set; }
        public string? SupplierState { get; set; }
        public string? SupplierAddress1 { get; set; }
        public string? SupplierAddress2 { get; set; }
        public string? SupplierTel { get; set; }
        public string? SupplierFax { get; set; }
        public string? SupplierEmail { get; set; }
        public int SupplierCloseDate { get; set; }
        public int SupplierPayMonths { get; set; }
        public int? SupplierPayDates { get; set; }
        public int SupplierPayMethod { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
