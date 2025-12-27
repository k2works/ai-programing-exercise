using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Infrastructure.Rest.Dto;

namespace ProductionManagement.Infrastructure.Rest.Controllers;

/// <summary>
/// 作業指示 Controller（Input Adapter）
/// </summary>
[ApiController]
[Route("api/work-orders")]
[Tags("Work Orders")]
public class WorkOrderController : ControllerBase
{
    private readonly IWorkOrderUseCase _workOrderUseCase;

    public WorkOrderController(IWorkOrderUseCase workOrderUseCase)
    {
        _workOrderUseCase = workOrderUseCase;
    }

    /// <summary>
    /// 作業指示一覧の取得
    /// </summary>
    [HttpGet]
    [ProducesResponseType(typeof(IReadOnlyList<WorkOrderResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetAllWorkOrders()
    {
        var workOrders = await _workOrderUseCase.GetAllWorkOrdersAsync();
        return Ok(workOrders.Select(WorkOrderResponse.FromWithoutDetails).ToList());
    }

    /// <summary>
    /// 作業指示の取得
    /// </summary>
    /// <param name="workOrderNumber">作業指示番号</param>
    [HttpGet("{workOrderNumber}")]
    [ProducesResponseType(typeof(WorkOrderResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> GetWorkOrder(string workOrderNumber)
    {
        var workOrder = await _workOrderUseCase.GetWorkOrderAsync(workOrderNumber);
        return Ok(WorkOrderResponse.From(workOrder));
    }

    /// <summary>
    /// 作業指示の作成
    /// </summary>
    [HttpPost]
    [ProducesResponseType(typeof(WorkOrderResponse), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> CreateWorkOrder([FromBody] CreateWorkOrderRequest request)
    {
        var command = new WorkOrderCreateCommand
        {
            OrderNumber = request.OrderNumber,
            WorkOrderDate = request.WorkOrderDate,
            LocationCode = request.LocationCode,
            PlannedStartDate = request.PlannedStartDate,
            PlannedEndDate = request.PlannedEndDate,
            Remarks = request.Remarks
        };

        var workOrder = await _workOrderUseCase.CreateWorkOrderAsync(command);
        return CreatedAtAction(
            nameof(GetWorkOrder),
            new { workOrderNumber = workOrder.WorkOrderNumber },
            WorkOrderResponse.From(workOrder)
        );
    }

    /// <summary>
    /// 作業の開始
    /// </summary>
    /// <param name="workOrderNumber">作業指示番号</param>
    [HttpPost("{workOrderNumber}/start")]
    [ProducesResponseType(typeof(WorkOrderResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> StartWork(string workOrderNumber)
    {
        var workOrder = await _workOrderUseCase.StartWorkAsync(workOrderNumber);
        return Ok(WorkOrderResponse.FromWithoutDetails(workOrder));
    }

    /// <summary>
    /// 作業の完了
    /// </summary>
    /// <param name="workOrderNumber">作業指示番号</param>
    [HttpPost("{workOrderNumber}/complete")]
    [ProducesResponseType(typeof(WorkOrderResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> CompleteWork(string workOrderNumber)
    {
        var workOrder = await _workOrderUseCase.CompleteWorkAsync(workOrderNumber);
        return Ok(WorkOrderResponse.FromWithoutDetails(workOrder));
    }
}
