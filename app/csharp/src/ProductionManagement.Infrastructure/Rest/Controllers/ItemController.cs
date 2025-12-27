using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Infrastructure.Rest.Dto;

namespace ProductionManagement.Infrastructure.Rest.Controllers;

/// <summary>
/// 品目 Controller（Input Adapter）
/// 既存の IItemUseCase を Input Adapter として呼び出す
/// </summary>
[ApiController]
[Route("api/items")]
[Tags("Items")]
public class ItemController : ControllerBase
{
    private readonly IItemUseCase _itemUseCase;

    public ItemController(IItemUseCase itemUseCase)
    {
        _itemUseCase = itemUseCase;
    }

    /// <summary>
    /// 品目一覧の取得
    /// </summary>
    /// <param name="category">品目区分でフィルタリング</param>
    [HttpGet]
    [ProducesResponseType(typeof(IReadOnlyList<ItemResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetAllItems([FromQuery] ItemCategory? category)
    {
        IReadOnlyList<Item> items;
        if (category.HasValue)
        {
            items = await _itemUseCase.GetItemsByCategoryAsync(category.Value);
        }
        else
        {
            items = await _itemUseCase.GetAllItemsAsync();
        }

        return Ok(items.Select(ItemResponse.From).ToList());
    }

    /// <summary>
    /// 品目の取得
    /// </summary>
    /// <param name="itemCode">品目コード</param>
    [HttpGet("{itemCode}")]
    [ProducesResponseType(typeof(ItemResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> GetItemByCode(string itemCode)
    {
        var item = await _itemUseCase.GetItemByCodeAsync(itemCode);
        return Ok(ItemResponse.From(item));
    }

    /// <summary>
    /// 品目の登録
    /// </summary>
    [HttpPost]
    [ProducesResponseType(typeof(ItemResponse), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status409Conflict)]
    public async Task<IActionResult> CreateItem([FromBody] CreateItemRequest request)
    {
        var command = new CreateItemCommand(
            ItemCode: request.ItemCode,
            ItemName: request.ItemName,
            Category: Enum.Parse<ItemCategory>(request.Category),
            UnitCode: request.UnitCode,
            LeadTime: request.LeadTime,
            SafetyLeadTime: request.SafetyLeadTime,
            SafetyStock: request.SafetyStock,
            YieldRate: request.YieldRate,
            MinLotSize: request.MinLotSize,
            LotIncrement: request.LotIncrement,
            MaxLotSize: request.MaxLotSize,
            ShelfLife: request.ShelfLife
        );

        var item = await _itemUseCase.CreateItemAsync(command);
        return CreatedAtAction(
            nameof(GetItemByCode),
            new { itemCode = item.ItemCode },
            ItemResponse.From(item)
        );
    }

    /// <summary>
    /// 品目の更新
    /// </summary>
    [HttpPut("{itemCode}")]
    [ProducesResponseType(typeof(ItemResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> UpdateItem(
        string itemCode,
        [FromBody] UpdateItemRequest request)
    {
        var command = new UpdateItemCommand(
            ItemCode: itemCode,
            ItemName: request.ItemName,
            Category: request.Category is not null
                ? Enum.Parse<ItemCategory>(request.Category)
                : null,
            UnitCode: request.UnitCode,
            LeadTime: request.LeadTime,
            SafetyLeadTime: request.SafetyLeadTime,
            SafetyStock: request.SafetyStock,
            YieldRate: request.YieldRate,
            MinLotSize: request.MinLotSize,
            LotIncrement: request.LotIncrement,
            MaxLotSize: request.MaxLotSize,
            ShelfLife: request.ShelfLife
        );

        var item = await _itemUseCase.UpdateItemAsync(command);
        return Ok(ItemResponse.From(item));
    }

    /// <summary>
    /// 品目の削除
    /// </summary>
    [HttpDelete("{itemCode}")]
    [ProducesResponseType(StatusCodes.Status204NoContent)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> DeleteItem(string itemCode)
    {
        await _itemUseCase.DeleteItemAsync(itemCode);
        return NoContent();
    }
}
