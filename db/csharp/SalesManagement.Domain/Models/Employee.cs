namespace SalesManagement.Domain.Models
{
    /// <summary>
    /// 社員マスタのEntityクラス
    /// </summary>
    public class Employee
    {
        public string EmployeeCode { get; set; } = string.Empty;
        public string EmployeeName { get; set; } = string.Empty;
        public string EmployeeNameKana { get; set; } = string.Empty;
        public string Gender { get; set; } = string.Empty;
        public DateTime? BirthDate { get; set; }
        public DateTime JoinDate { get; set; }
        public string DepartmentCode { get; set; } = string.Empty;
        public string? PositionCode { get; set; }
        public DateTime CreatedAt { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime UpdatedAt { get; set; }
        public string UpdatedBy { get; set; } = string.Empty;
    }
}
