using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Models.Inventory;
using ProductionManagement.Infrastructure.Rest.Dto;

namespace ProductionManagement.Infrastructure.Rest.Controllers;

/// <summary>
/// 在庫照会 Controller（Input Adapter）
/// </summary>
[ApiController]
[Route("api/inventory")]
[Tags("Inventory")]
public class InventoryController : ControllerBase
{
    private readonly IInventoryUseCase _inventoryUseCase;

    public InventoryController(IInventoryUseCase inventoryUseCase)
    {
        _inventoryUseCase = inventoryUseCase;
    }

    /// <summary>
    /// 在庫一覧の取得
    /// </summary>
    /// <param name="itemCode">品目コード（フィルター）</param>
    /// <param name="locationCode">場所コード（フィルター）</param>
    /// <param name="status">在庫ステータス（フィルター）</param>
    [HttpGet]
    [ProducesResponseType(typeof(IReadOnlyList<StockResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetInventory(
        [FromQuery] string? itemCode,
        [FromQuery] string? locationCode,
        [FromQuery] StockStatus? status)
    {
        var query = new InventoryQuery(itemCode, locationCode, status);
        var stocks = await _inventoryUseCase.GetInventoryAsync(query);
        return Ok(stocks.Select(StockResponse.From).ToList());
    }

    /// <summary>
    /// 在庫サマリーの取得
    /// </summary>
    [HttpGet("summary")]
    [ProducesResponseType(typeof(IReadOnlyList<InventorySummaryResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetInventorySummary()
    {
        var summaries = await _inventoryUseCase.GetInventorySummaryAsync();
        return Ok(summaries.Select(InventorySummaryResponse.From).ToList());
    }

    /// <summary>
    /// 在庫不足品目の取得
    /// </summary>
    [HttpGet("shortage")]
    [ProducesResponseType(typeof(IReadOnlyList<InventorySummaryResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetShortageItems()
    {
        var items = await _inventoryUseCase.GetShortageItemsAsync();
        return Ok(items.Select(InventorySummaryResponse.From).ToList());
    }
}
