using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Infrastructure.Rest.Dto;

namespace ProductionManagement.Infrastructure.Rest.Controllers;

/// <summary>
/// 発注 Controller（Input Adapter）
/// </summary>
[ApiController]
[Route("api/purchase-orders")]
[Tags("Purchase Orders")]
public class PurchaseOrderController : ControllerBase
{
    private readonly IPurchaseOrderUseCase _purchaseOrderUseCase;

    public PurchaseOrderController(IPurchaseOrderUseCase purchaseOrderUseCase)
    {
        _purchaseOrderUseCase = purchaseOrderUseCase;
    }

    /// <summary>
    /// 発注一覧の取得
    /// </summary>
    [HttpGet]
    [ProducesResponseType(typeof(IReadOnlyList<PurchaseOrderResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetAllOrders()
    {
        var orders = await _purchaseOrderUseCase.GetAllOrdersAsync();
        return Ok(orders.Select(PurchaseOrderResponse.FromWithoutDetails).ToList());
    }

    /// <summary>
    /// 発注の取得
    /// </summary>
    /// <param name="purchaseOrderNumber">発注番号</param>
    [HttpGet("{purchaseOrderNumber}")]
    [ProducesResponseType(typeof(PurchaseOrderResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> GetOrder(string purchaseOrderNumber)
    {
        var order = await _purchaseOrderUseCase.GetOrderAsync(purchaseOrderNumber);
        return Ok(PurchaseOrderResponse.From(order));
    }

    /// <summary>
    /// 発注の作成
    /// </summary>
    [HttpPost]
    [ProducesResponseType(typeof(PurchaseOrderResponse), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> CreateOrder([FromBody] CreatePurchaseOrderRequest request)
    {
        var command = new PurchaseOrderCreateCommand
        {
            SupplierCode = request.SupplierCode,
            OrderDate = request.OrderDate,
            OrdererCode = request.OrdererCode,
            DepartmentCode = request.DepartmentCode,
            TaxRate = request.TaxRate,
            Remarks = request.Remarks,
            Details = request.Details.Select(d => new PurchaseOrderDetailCommand
            {
                ItemCode = d.ItemCode,
                OrderQuantity = d.OrderQuantity,
                ExpectedReceivingDate = d.ExpectedReceivingDate,
                OrderNumber = d.OrderNumber,
                DeliveryLocationCode = d.DeliveryLocationCode
            }).ToList()
        };

        var order = await _purchaseOrderUseCase.CreateOrderAsync(command);
        return CreatedAtAction(
            nameof(GetOrder),
            new { purchaseOrderNumber = order.PurchaseOrderNumber },
            PurchaseOrderResponse.From(order)
        );
    }

    /// <summary>
    /// 発注の確定
    /// </summary>
    /// <param name="purchaseOrderNumber">発注番号</param>
    [HttpPost("{purchaseOrderNumber}/confirm")]
    [ProducesResponseType(typeof(PurchaseOrderResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> ConfirmOrder(string purchaseOrderNumber)
    {
        var order = await _purchaseOrderUseCase.ConfirmOrderAsync(purchaseOrderNumber);
        return Ok(PurchaseOrderResponse.FromWithoutDetails(order));
    }

    /// <summary>
    /// 発注の取消
    /// </summary>
    /// <param name="purchaseOrderNumber">発注番号</param>
    [HttpPost("{purchaseOrderNumber}/cancel")]
    [ProducesResponseType(StatusCodes.Status204NoContent)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> CancelOrder(string purchaseOrderNumber)
    {
        await _purchaseOrderUseCase.CancelOrderAsync(purchaseOrderNumber);
        return NoContent();
    }
}
