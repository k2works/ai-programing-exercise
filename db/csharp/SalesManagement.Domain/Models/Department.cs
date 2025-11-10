namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 部門マスタのEntityクラス
    /// </summary>
    public class Department
    {
        public string DepartmentCode { get; set; } = string.Empty;
        public DateTime StartDate { get; set; }
        public DateTime EndDate { get; set; }
        public string DepartmentName { get; set; } = string.Empty;
        public int OrganizationLevel { get; set; }
        public string DepartmentPath { get; set; } = string.Empty;
        public int LowestLevelFlag { get; set; }
        public int SlipInputFlag { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
