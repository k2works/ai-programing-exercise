using ProductionManagement.Domain.Models.Master;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 部門リポジトリインターフェース
/// </summary>
public interface IDepartmentRepository
{
    Task SaveAsync(Department department);
    Task<Department?> FindByDepartmentCodeAsync(string departmentCode);
    Task DeleteAllAsync();
}
