namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 取引先マスタ（Partyモデルの基盤）
    /// </summary>
    public class Company
    {
        public string CompanyCode { get; set; } = string.Empty;
        public string CompanyName { get; set; } = string.Empty;
        public string? CompanyNameKana { get; set; }
        public int SupplierType { get; set; }
        public string? ZipCode { get; set; }
        public string? State { get; set; }
        public string? Address1 { get; set; }
        public string? Address2 { get; set; }
        public int NoSalesFlag { get; set; }
        public int WideUseType { get; set; }
        public string CompanyGroupCode { get; set; } = string.Empty;
        public int MaxCredit { get; set; }
        public int TempCreditUp { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
