using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Application.Port.In;

/// <summary>
/// 作業指示ユースケース（Input Port）
/// </summary>
public interface IWorkOrderUseCase
{
    /// <summary>
    /// 作業指示を作成する
    /// </summary>
    Task<WorkOrder> CreateWorkOrderAsync(WorkOrderCreateCommand command);

    /// <summary>
    /// 作業指示を取得する
    /// </summary>
    Task<WorkOrder> GetWorkOrderAsync(string workOrderNumber);

    /// <summary>
    /// 作業指示一覧を取得する
    /// </summary>
    Task<IReadOnlyList<WorkOrder>> GetAllWorkOrdersAsync();

    /// <summary>
    /// 作業を開始する
    /// </summary>
    Task<WorkOrder> StartWorkAsync(string workOrderNumber);

    /// <summary>
    /// 作業を完了する
    /// </summary>
    Task<WorkOrder> CompleteWorkAsync(string workOrderNumber);
}
