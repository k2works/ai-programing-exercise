using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 在庫サービス
/// </summary>
public class InventoryService : IInventoryUseCase
{
    private readonly IStockRepository _stockRepository;
    private readonly IItemRepository _itemRepository;

    public InventoryService(IStockRepository stockRepository, IItemRepository itemRepository)
    {
        _stockRepository = stockRepository;
        _itemRepository = itemRepository;
    }

    /// <summary>
    /// 在庫一覧を取得
    /// </summary>
    public async Task<IReadOnlyList<Stock>> GetInventoryAsync(InventoryQuery query)
    {
        var stocks = await _stockRepository.FindAllAsync();

        return stocks
            .Where(s => query.ItemCode == null || s.ItemCode == query.ItemCode)
            .Where(s => query.LocationCode == null || s.LocationCode == query.LocationCode)
            .Where(s => query.Status == null || HasStockByStatus(s, query.Status.Value))
            .ToList();
    }

    private static bool HasStockByStatus(Stock stock, StockStatus status) => status switch
    {
        StockStatus.Passed => stock.PassedQuantity > 0,
        StockStatus.Defective => stock.DefectiveQuantity > 0,
        StockStatus.Uninspected => stock.UninspectedQuantity > 0,
        _ => false
    };

    /// <summary>
    /// 在庫サマリーを取得
    /// </summary>
    public async Task<IReadOnlyList<InventorySummary>> GetInventorySummaryAsync()
    {
        var stocks = await _stockRepository.FindAllAsync();
        var items = await _itemRepository.FindAllAsync();
        var itemDict = items.ToDictionary(i => i.ItemCode);

        var grouped = stocks.GroupBy(s => s.ItemCode);
        var summaries = new List<InventorySummary>();

        foreach (var group in grouped)
        {
            var itemCode = group.Key;
            var totalQuantity = group.Sum(s => s.StockQuantity);
            var item = itemDict.GetValueOrDefault(itemCode);
            var safetyStock = item?.SafetyStock;

            var state = DetermineStockState(totalQuantity, safetyStock);

            summaries.Add(new InventorySummary(
                itemCode,
                item?.ItemName ?? itemCode,
                totalQuantity,
                safetyStock,
                state
            ));
        }

        return summaries;
    }

    /// <summary>
    /// 在庫不足品目を取得
    /// </summary>
    public async Task<IReadOnlyList<InventorySummary>> GetShortageItemsAsync()
    {
        var summaries = await GetInventorySummaryAsync();
        return summaries.Where(s => s.StockState == StockState.Shortage).ToList();
    }

    private static StockState DetermineStockState(decimal totalQuantity, decimal? safetyStock)
    {
        if (safetyStock == null)
        {
            return StockState.Normal;
        }

        if (totalQuantity < safetyStock.Value)
        {
            return StockState.Shortage;
        }

        if (totalQuantity > safetyStock.Value * 2)
        {
            return StockState.Excess;
        }

        return StockState.Normal;
    }

    /// <summary>
    /// 在庫を取得する
    /// </summary>
    public async Task<Stock> GetStockAsync(string locationCode, string itemCode)
    {
        var stock = await _stockRepository.FindByLocationAndItemAsync(locationCode, itemCode);
        return stock ?? Stock.Empty(locationCode, itemCode);
    }

    /// <summary>
    /// 在庫を増加する
    /// </summary>
    public async Task IncreaseStockAsync(StockChangeCommand command)
    {
        var stock = await _stockRepository.FindByLocationAndItemAsync(command.LocationCode, command.ItemCode);

        if (stock is null)
        {
            // 新規作成
            var newStock = new Stock
            {
                LocationCode = command.LocationCode,
                ItemCode = command.ItemCode,
                StockQuantity = command.Quantity,
                PassedQuantity = command.StockStatus == StockStatus.Passed ? command.Quantity : 0m,
                DefectiveQuantity = command.StockStatus == StockStatus.Defective ? command.Quantity : 0m,
                UninspectedQuantity = command.StockStatus == StockStatus.Uninspected ? command.Quantity : 0m
            };
            await _stockRepository.SaveAsync(newStock);
        }
        else
        {
            // 更新
            await _stockRepository.IncreaseByStatusAsync(
                command.LocationCode,
                command.ItemCode,
                command.Quantity,
                command.StockStatus);
        }
    }

    /// <summary>
    /// 在庫を減少する
    /// </summary>
    public async Task DecreaseStockAsync(StockChangeCommand command)
    {
        var stock = await GetStockAsync(command.LocationCode, command.ItemCode);
        var currentQuantity = GetQuantityByStatus(stock, command.StockStatus);

        if (currentQuantity < command.Quantity)
        {
            throw new InsufficientStockException("在庫が不足しています");
        }

        await _stockRepository.DecreaseByStatusAsync(
            command.LocationCode,
            command.ItemCode,
            command.Quantity,
            command.StockStatus);
    }

    /// <summary>
    /// 在庫状態を変更する
    /// </summary>
    public async Task ChangeStockStatusAsync(StockStatusChangeCommand command)
    {
        var stock = await GetStockAsync(command.LocationCode, command.ItemCode);
        var currentQuantity = GetQuantityByStatus(stock, command.FromStatus);

        if (currentQuantity < command.Quantity)
        {
            throw new InsufficientStockException($"{command.FromStatus.ToDisplayName()}の在庫が不足しています");
        }

        await _stockRepository.ChangeStatusAsync(
            command.LocationCode,
            command.ItemCode,
            command.Quantity,
            command.FromStatus,
            command.ToStatus);
    }

    private static decimal GetQuantityByStatus(Stock stock, StockStatus status) => status switch
    {
        StockStatus.Passed => stock.PassedQuantity,
        StockStatus.Defective => stock.DefectiveQuantity,
        StockStatus.Uninspected => stock.UninspectedQuantity,
        _ => throw new ArgumentOutOfRangeException(nameof(status))
    };
}
