using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 工数実績リポジトリインターフェース
/// </summary>
public interface ILaborHoursRepository
{
    Task SaveAsync(LaborHours laborHours);
    Task<LaborHours?> FindByLaborHoursNumberAsync(string laborHoursNumber);
    Task<IReadOnlyList<LaborHours>> FindByWorkOrderNumberAsync(string workOrderNumber);
    Task<decimal> SumByWorkOrderAndSequenceAsync(string workOrderNumber, int sequence);
    Task<decimal> SumByWorkOrderAsync(string workOrderNumber);
    Task<decimal> SumByEmployeeAsync(string employeeCode, DateOnly startDate, DateOnly endDate);
    Task<string?> FindLatestLaborHoursNumberAsync(string prefix);
    Task DeleteAllAsync();
}
