using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Infrastructure.Rest.Dto;

namespace ProductionManagement.Infrastructure.Rest.Controllers;

/// <summary>
/// オーダ Controller（Input Adapter）
/// </summary>
[ApiController]
[Route("api/orders")]
[Tags("Orders")]
public class OrderController : ControllerBase
{
    private readonly IOrderUseCase _orderUseCase;

    public OrderController(IOrderUseCase orderUseCase)
    {
        _orderUseCase = orderUseCase;
    }

    /// <summary>
    /// オーダの取得（ID指定）
    /// </summary>
    /// <param name="orderId">オーダID</param>
    [HttpGet("{orderId:int}")]
    [ProducesResponseType(typeof(OrderResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> GetOrderById(int orderId)
    {
        var order = await _orderUseCase.GetOrderByIdAsync(orderId);
        return Ok(OrderResponse.From(order));
    }

    /// <summary>
    /// オーダの取得（オーダ番号指定）
    /// </summary>
    /// <param name="orderNumber">オーダ番号</param>
    [HttpGet("by-number/{orderNumber}")]
    [ProducesResponseType(typeof(OrderResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> GetOrderByNumber(string orderNumber)
    {
        var order = await _orderUseCase.GetOrderByNumberAsync(orderNumber);
        return Ok(OrderResponse.From(order));
    }

    /// <summary>
    /// MPSに紐づくオーダの取得
    /// </summary>
    /// <param name="mpsId">MPS ID</param>
    [HttpGet("by-mps/{mpsId:int}")]
    [ProducesResponseType(typeof(IReadOnlyList<OrderResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetOrdersByMpsId(int mpsId)
    {
        var orders = await _orderUseCase.GetOrdersByMpsIdAsync(mpsId);
        return Ok(orders.Select(OrderResponse.From).ToList());
    }

    /// <summary>
    /// オーダの登録
    /// </summary>
    [HttpPost]
    [ProducesResponseType(typeof(OrderResponse), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> CreateOrder([FromBody] CreateOrderRequest request)
    {
        var orderType = OrderTypeExtensions.FromDisplayName(request.OrderType);
        var command = new CreateOrderCommand(
            OrderType: orderType,
            ItemCode: request.ItemCode,
            StartDate: request.StartDate,
            DueDate: request.DueDate,
            PlanQuantity: request.PlanQuantity,
            LocationCode: request.LocationCode,
            MpsId: request.MpsId,
            ParentOrderId: request.ParentOrderId,
            ExpirationDate: request.ExpirationDate
        );

        var order = await _orderUseCase.CreateOrderAsync(command);
        return CreatedAtAction(
            nameof(GetOrderById),
            new { orderId = order.Id },
            OrderResponse.From(order)
        );
    }

    /// <summary>
    /// オーダの確定
    /// </summary>
    /// <param name="orderId">オーダID</param>
    [HttpPost("{orderId:int}/confirm")]
    [ProducesResponseType(typeof(OrderResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> ConfirmOrder(int orderId)
    {
        var order = await _orderUseCase.ConfirmOrderAsync(orderId);
        return Ok(OrderResponse.From(order));
    }

    /// <summary>
    /// オーダのキャンセル
    /// </summary>
    /// <param name="orderId">オーダID</param>
    [HttpPost("{orderId:int}/cancel")]
    [ProducesResponseType(StatusCodes.Status204NoContent)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> CancelOrder(int orderId)
    {
        await _orderUseCase.CancelOrderAsync(orderId);
        return NoContent();
    }
}
