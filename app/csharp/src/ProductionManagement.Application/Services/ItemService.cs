using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Item;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 品目アプリケーションサービス
/// </summary>
public class ItemService : IItemUseCase
{
    private readonly IItemRepository _itemRepository;

    public ItemService(IItemRepository itemRepository)
    {
        _itemRepository = itemRepository;
    }

    public async Task<Item> CreateItemAsync(CreateItemCommand command)
    {
        // 重複チェック
        var existing = await _itemRepository.FindByItemCodeAsync(command.ItemCode);
        if (existing is not null)
        {
            throw new DuplicateItemException(command.ItemCode);
        }

        var item = new Item
        {
            ItemCode = command.ItemCode,
            EffectiveFrom = DateOnly.FromDateTime(DateTime.Today),
            ItemName = command.ItemName,
            ItemCategory = command.Category,
            UnitCode = command.UnitCode,
            LeadTime = command.LeadTime,
            SafetyLeadTime = command.SafetyLeadTime,
            SafetyStock = command.SafetyStock,
            YieldRate = command.YieldRate,
            MinLotSize = command.MinLotSize,
            LotIncrement = command.LotIncrement,
            MaxLotSize = command.MaxLotSize,
            ShelfLife = command.ShelfLife
        };

        await _itemRepository.SaveAsync(item);
        return item;
    }

    public async Task<Item> UpdateItemAsync(UpdateItemCommand command)
    {
        var item = await _itemRepository.FindByItemCodeAsync(command.ItemCode)
            ?? throw new ItemNotFoundException(command.ItemCode);

        // 更新処理
        if (command.ItemName is not null)
        {
            item.ItemName = command.ItemName;
        }

        if (command.Category.HasValue)
        {
            item.ItemCategory = command.Category.Value;
        }

        if (command.UnitCode is not null)
        {
            item.UnitCode = command.UnitCode;
        }

        if (command.LeadTime.HasValue)
        {
            item.LeadTime = command.LeadTime.Value;
        }

        if (command.SafetyLeadTime.HasValue)
        {
            item.SafetyLeadTime = command.SafetyLeadTime.Value;
        }

        if (command.SafetyStock.HasValue)
        {
            item.SafetyStock = command.SafetyStock.Value;
        }

        if (command.YieldRate.HasValue)
        {
            item.YieldRate = command.YieldRate.Value;
        }

        if (command.MinLotSize.HasValue)
        {
            item.MinLotSize = command.MinLotSize.Value;
        }

        if (command.LotIncrement.HasValue)
        {
            item.LotIncrement = command.LotIncrement.Value;
        }

        if (command.MaxLotSize.HasValue)
        {
            item.MaxLotSize = command.MaxLotSize.Value;
        }

        if (command.ShelfLife.HasValue)
        {
            item.ShelfLife = command.ShelfLife.Value;
        }

        await _itemRepository.UpdateAsync(item);
        return item;
    }

    public async Task<IReadOnlyList<Item>> GetAllItemsAsync()
    {
        return await _itemRepository.FindAllAsync();
    }

    public async Task<Item> GetItemByCodeAsync(string itemCode)
    {
        return await _itemRepository.FindByItemCodeAsync(itemCode)
            ?? throw new ItemNotFoundException(itemCode);
    }

    public async Task<IReadOnlyList<Item>> GetItemsByCategoryAsync(ItemCategory category)
    {
        return await _itemRepository.FindByCategoryAsync(category);
    }

    public async Task DeleteItemAsync(string itemCode)
    {
        _ = await _itemRepository.FindByItemCodeAsync(itemCode)
            ?? throw new ItemNotFoundException(itemCode);

        await _itemRepository.DeleteByCodeAsync(itemCode);
    }

    public async Task<IReadOnlyList<Item>> SearchItemsAsync(string keyword)
    {
        if (string.IsNullOrWhiteSpace(keyword))
        {
            return await _itemRepository.FindAllAsync();
        }

        return await _itemRepository.SearchAsync(keyword);
    }
}
