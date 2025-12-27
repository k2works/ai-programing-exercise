using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 作業指示リポジトリインターフェース
/// </summary>
public interface IWorkOrderRepository
{
    Task SaveAsync(WorkOrder workOrder);
    Task<WorkOrder?> FindByWorkOrderNumberAsync(string workOrderNumber);
    Task<string?> FindLatestWorkOrderNumberAsync(string prefix);
    Task StartWorkAsync(string workOrderNumber, DateOnly actualStartDate);
    Task CompleteWorkAsync(string workOrderNumber, DateOnly actualEndDate);
    Task UpdateCompletionQuantitiesAsync(string workOrderNumber, decimal completedQuantity, decimal goodQuantity, decimal defectQuantity);
    Task DeleteAllAsync();
}
