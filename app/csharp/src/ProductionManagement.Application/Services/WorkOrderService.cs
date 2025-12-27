using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 作業指示サービス
/// </summary>
public class WorkOrderService : IWorkOrderUseCase
{
    private readonly IWorkOrderRepository _workOrderRepository;
    private readonly IWorkOrderDetailRepository _workOrderDetailRepository;
    private readonly IOrderRepository _orderRepository;
    private readonly IRoutingRepository _routingRepository;

    public WorkOrderService(
        IWorkOrderRepository workOrderRepository,
        IWorkOrderDetailRepository workOrderDetailRepository,
        IOrderRepository orderRepository,
        IRoutingRepository routingRepository)
    {
        _workOrderRepository = workOrderRepository;
        _workOrderDetailRepository = workOrderDetailRepository;
        _orderRepository = orderRepository;
        _routingRepository = routingRepository;
    }

    /// <summary>
    /// 作業指示番号を生成する
    /// </summary>
    private async Task<string> GenerateWorkOrderNumberAsync(DateOnly workOrderDate)
    {
        var prefix = $"WO-{workOrderDate:yyyyMM}-";
        var latestNumber = await _workOrderRepository.FindLatestWorkOrderNumberAsync($"{prefix}%");

        var sequence = 1;
        if (latestNumber != null)
        {
            var currentSequence = int.Parse(latestNumber[^4..]);
            sequence = currentSequence + 1;
        }

        return $"{prefix}{sequence:D4}";
    }

    /// <summary>
    /// 作業指示を作成する
    /// </summary>
    public async Task<WorkOrder> CreateWorkOrderAsync(WorkOrderCreateCommand command)
    {
        // オーダ情報を取得
        var order = await _orderRepository.FindByOrderNumberAsync(command.OrderNumber)
            ?? throw new ArgumentException($"Order not found: {command.OrderNumber}");

        // 工程表を取得
        var routings = await _routingRepository.FindByItemCodeAsync(order.ItemCode);
        if (routings.Count == 0)
        {
            throw new ArgumentException($"Routing not found for item: {order.ItemCode}");
        }

        var workOrderNumber = await GenerateWorkOrderNumberAsync(command.WorkOrderDate);

        // 作業指示ヘッダを作成
        var workOrder = new WorkOrder
        {
            WorkOrderNumber = workOrderNumber,
            OrderNumber = command.OrderNumber,
            WorkOrderDate = command.WorkOrderDate,
            ItemCode = order.ItemCode,
            OrderQuantity = order.PlanQuantity,
            LocationCode = command.LocationCode,
            PlannedStartDate = command.PlannedStartDate,
            PlannedEndDate = command.PlannedEndDate,
            CompletedQuantity = 0m,
            TotalGoodQuantity = 0m,
            TotalDefectQuantity = 0m,
            Status = WorkOrderStatus.NotStarted,
            CompletedFlag = false,
            Remarks = command.Remarks
        };
        await _workOrderRepository.SaveAsync(workOrder);

        // 作業指示明細を作成
        var details = new List<WorkOrderDetail>();
        foreach (var routing in routings)
        {
            var detail = new WorkOrderDetail
            {
                WorkOrderNumber = workOrderNumber,
                Sequence = routing.Sequence,
                ProcessCode = routing.ProcessCode
            };
            await _workOrderDetailRepository.SaveAsync(detail);
            details.Add(detail);
        }

        workOrder.Details = details;
        return workOrder;
    }

    /// <summary>
    /// 作業を開始する
    /// </summary>
    public async Task<WorkOrder> StartWorkAsync(string workOrderNumber)
    {
        var workOrder = await _workOrderRepository.FindByWorkOrderNumberAsync(workOrderNumber)
            ?? throw new ArgumentException($"Work order not found: {workOrderNumber}");

        if (workOrder.Status != WorkOrderStatus.NotStarted)
        {
            throw new InvalidOperationException("Only NOT_STARTED work orders can be started");
        }

        await _workOrderRepository.StartWorkAsync(workOrderNumber, DateOnly.FromDateTime(DateTime.Today));
        return await _workOrderRepository.FindByWorkOrderNumberAsync(workOrderNumber)
            ?? throw new InvalidOperationException("Work order not found after update");
    }

    /// <summary>
    /// 作業を完了する
    /// </summary>
    public async Task<WorkOrder> CompleteWorkAsync(string workOrderNumber)
    {
        var workOrder = await _workOrderRepository.FindByWorkOrderNumberAsync(workOrderNumber)
            ?? throw new ArgumentException($"Work order not found: {workOrderNumber}");

        if (workOrder.Status != WorkOrderStatus.InProgress)
        {
            throw new InvalidOperationException("Only IN_PROGRESS work orders can be completed");
        }

        await _workOrderRepository.CompleteWorkAsync(workOrderNumber, DateOnly.FromDateTime(DateTime.Today));
        return await _workOrderRepository.FindByWorkOrderNumberAsync(workOrderNumber)
            ?? throw new InvalidOperationException("Work order not found after update");
    }

    /// <summary>
    /// 作業指示を取得する
    /// </summary>
    public async Task<WorkOrder> GetWorkOrderAsync(string workOrderNumber)
    {
        var workOrder = await _workOrderRepository.FindByWorkOrderNumberAsync(workOrderNumber)
            ?? throw new WorkOrderNotFoundException(workOrderNumber);

        var details = await _workOrderDetailRepository.FindByWorkOrderNumberAsync(workOrderNumber);
        workOrder.Details = details;
        return workOrder;
    }

    /// <summary>
    /// 作業指示一覧を取得する
    /// </summary>
    public async Task<IReadOnlyList<WorkOrder>> GetAllWorkOrdersAsync()
    {
        return await _workOrderRepository.FindAllAsync();
    }
}
