using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 棚卸サービス
/// </summary>
public class StocktakingService
{
    private readonly IStocktakingRepository _stocktakingRepository;
    private readonly IStockAdjustmentRepository _stockAdjustmentRepository;
    private readonly IStockRepository _stockRepository;

    public StocktakingService(
        IStocktakingRepository stocktakingRepository,
        IStockAdjustmentRepository stockAdjustmentRepository,
        IStockRepository stockRepository)
    {
        _stocktakingRepository = stocktakingRepository;
        _stockAdjustmentRepository = stockAdjustmentRepository;
        _stockRepository = stockRepository;
    }

    /// <summary>
    /// 棚卸表を発行する
    /// </summary>
    public async Task<Stocktaking> IssueStocktakingSheetAsync(StocktakingIssueCommand command)
    {
        // 棚卸番号を採番
        var stocktakingNumber = await GenerateStocktakingNumberAsync(command.StocktakingDate);

        // 対象場所の在庫を取得
        var stocks = await _stockRepository.FindByLocationAsync(command.LocationCode);

        // 棚卸データを作成
        var stocktaking = new Stocktaking
        {
            StocktakingNumber = stocktakingNumber,
            LocationCode = command.LocationCode,
            StocktakingDate = command.StocktakingDate,
            Status = StocktakingStatus.Issued
        };

        await _stocktakingRepository.SaveAsync(stocktaking);

        // 棚卸明細データを作成
        var lineNumber = 1;
        foreach (var stock in stocks)
        {
            var detail = new StocktakingDetail
            {
                StocktakingNumber = stocktakingNumber,
                LineNumber = lineNumber++,
                ItemCode = stock.ItemCode,
                BookQuantity = stock.StockQuantity
            };
            await _stocktakingRepository.SaveDetailAsync(detail);
        }

        // 作成した棚卸データを返却
        var result = await _stocktakingRepository.FindByStocktakingNumberAsync(stocktakingNumber);
        result!.Details = await _stocktakingRepository.FindDetailsByStocktakingNumberAsync(stocktakingNumber);

        return result;
    }

    /// <summary>
    /// 実棚数量を入力する
    /// </summary>
    public async Task InputActualCountAsync(ActualCountInputCommand command)
    {
        var stocktaking = await _stocktakingRepository.FindByStocktakingNumberAsync(command.StocktakingNumber);
        if (stocktaking is null)
        {
            throw new ResourceNotFoundException($"棚卸データが見つかりません: {command.StocktakingNumber}");
        }

        if (stocktaking.Status != StocktakingStatus.Issued)
        {
            throw new InvalidOperationException($"発行済みの棚卸データのみ入力可能です。現在のステータス: {stocktaking.Status.ToDisplayName()}");
        }

        var details = await _stocktakingRepository.FindDetailsByStocktakingNumberAsync(command.StocktakingNumber);

        foreach (var input in command.Details)
        {
            var detail = details.FirstOrDefault(d => d.ItemCode == input.ItemCode);
            if (detail is null)
            {
                throw new ResourceNotFoundException($"棚卸明細が見つかりません: 品目コード {input.ItemCode}");
            }

            var difference = input.ActualQuantity - detail.BookQuantity;
            await _stocktakingRepository.UpdateDetailAsync(detail.Id!.Value, input.ActualQuantity, difference);
        }

        // ステータスを入力済みに更新
        await _stocktakingRepository.UpdateStatusAsync(command.StocktakingNumber, StocktakingStatus.Entered);
    }

    /// <summary>
    /// 棚卸を確定する
    /// </summary>
    public async Task ConfirmStocktakingAsync(StocktakingConfirmCommand command)
    {
        var stocktaking = await _stocktakingRepository.FindByStocktakingNumberAsync(command.StocktakingNumber);
        if (stocktaking is null)
        {
            throw new ResourceNotFoundException($"棚卸データが見つかりません: {command.StocktakingNumber}");
        }

        if (stocktaking.Status != StocktakingStatus.Entered)
        {
            throw new InvalidOperationException($"入力済みの棚卸データのみ確定可能です。現在のステータス: {stocktaking.Status.ToDisplayName()}");
        }

        var details = await _stocktakingRepository.FindDetailsByStocktakingNumberAsync(command.StocktakingNumber);

        // 差異がある明細に対して在庫調整を実施
        var adjustmentSequence = 1;
        foreach (var detail in details.Where(d => d.DifferenceQuantity != 0))
        {
            var adjustmentNumber = $"ADJ-{command.StocktakingNumber}-{adjustmentSequence++:D3}";

            var adjustment = new StockAdjustment
            {
                AdjustmentNumber = adjustmentNumber,
                StocktakingNumber = command.StocktakingNumber,
                ItemCode = detail.ItemCode,
                LocationCode = stocktaking.LocationCode,
                AdjustmentDate = stocktaking.StocktakingDate,
                AdjusterCode = command.AdjusterCode,
                AdjustmentQuantity = detail.DifferenceQuantity!.Value,
                ReasonCode = command.AdjustmentReasonCode
            };

            await _stockAdjustmentRepository.SaveAsync(adjustment);

            // 在庫を調整（差異分を増減）
            var stock = await _stockRepository.FindByLocationAndItemAsync(stocktaking.LocationCode, detail.ItemCode);
            if (stock is not null)
            {
                if (detail.DifferenceQuantity > 0)
                {
                    // 実棚 > 帳簿：在庫増加
                    await _stockRepository.IncreaseByStatusAsync(
                        stocktaking.LocationCode,
                        detail.ItemCode,
                        detail.DifferenceQuantity!.Value,
                        StockStatus.Passed);
                }
                else
                {
                    // 実棚 < 帳簿：在庫減少
                    await _stockRepository.DecreaseByStatusAsync(
                        stocktaking.LocationCode,
                        detail.ItemCode,
                        Math.Abs(detail.DifferenceQuantity!.Value),
                        StockStatus.Passed);
                }
            }
        }

        // ステータスを確定に更新
        await _stocktakingRepository.UpdateStatusAsync(command.StocktakingNumber, StocktakingStatus.Confirmed);
    }

    /// <summary>
    /// 棚卸番号を採番する
    /// </summary>
    private async Task<string> GenerateStocktakingNumberAsync(DateOnly date)
    {
        var prefix = $"ST-{date:yyyyMM}-";
        var pattern = $"{prefix}%";

        var latestNumber = await _stocktakingRepository.FindLatestStocktakingNumberAsync(pattern);

        if (latestNumber is null)
        {
            return $"{prefix}0001";
        }

        var sequencePart = latestNumber[prefix.Length..];
        var sequence = int.Parse(sequencePart, System.Globalization.CultureInfo.InvariantCulture) + 1;
        return $"{prefix}{sequence:D4}";
    }
}
