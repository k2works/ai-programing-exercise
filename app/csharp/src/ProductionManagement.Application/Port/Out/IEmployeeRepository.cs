using ProductionManagement.Domain.Models.Master;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 担当者リポジトリインターフェース
/// </summary>
public interface IEmployeeRepository
{
    Task SaveAsync(Employee employee);
    Task<Employee?> FindByEmployeeCodeAsync(string employeeCode);
    Task<IReadOnlyList<Employee>> FindByDepartmentCodeAsync(string departmentCode);
    Task DeleteAllAsync();
}
