using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 作業指示明細リポジトリインターフェース
/// </summary>
public interface IWorkOrderDetailRepository
{
    Task SaveAsync(WorkOrderDetail detail);
    Task<IReadOnlyList<WorkOrderDetail>> FindByWorkOrderNumberAsync(string workOrderNumber);
    Task<WorkOrderDetail?> FindByWorkOrderAndSequenceAsync(string workOrderNumber, int sequence);
    Task DeleteAllAsync();
}
