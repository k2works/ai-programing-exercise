using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 在庫サービス
/// </summary>
public class InventoryService
{
    private readonly IStockRepository _stockRepository;

    public InventoryService(IStockRepository stockRepository)
    {
        _stockRepository = stockRepository;
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
