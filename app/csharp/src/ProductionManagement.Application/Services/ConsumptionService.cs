using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Subcontract;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 消費サービス
/// </summary>
public class ConsumptionService
{
    private readonly IConsumptionRepository _consumptionRepository;
    private readonly IConsumptionDetailRepository _consumptionDetailRepository;
    private readonly IReceivingRepository _receivingRepository;
    private readonly ISupplyRepository _supplyRepository;
    private readonly ISupplyDetailRepository _supplyDetailRepository;

    public ConsumptionService(
        IConsumptionRepository consumptionRepository,
        IConsumptionDetailRepository consumptionDetailRepository,
        IReceivingRepository receivingRepository,
        ISupplyRepository supplyRepository,
        ISupplyDetailRepository supplyDetailRepository)
    {
        _consumptionRepository = consumptionRepository;
        _consumptionDetailRepository = consumptionDetailRepository;
        _receivingRepository = receivingRepository;
        _supplyRepository = supplyRepository;
        _supplyDetailRepository = supplyDetailRepository;
    }

    /// <summary>
    /// 消費番号を生成する
    /// </summary>
    private async Task<string> GenerateConsumptionNumberAsync(DateOnly consumptionDate)
    {
        var prefix = $"CON-{consumptionDate:yyyyMM}-";
        var latestNumber = await _consumptionRepository.FindLatestConsumptionNumberAsync($"{prefix}%");

        var sequence = 1;
        if (latestNumber != null)
        {
            var currentSequence = int.Parse(latestNumber[^4..]);
            sequence = currentSequence + 1;
        }

        return $"{prefix}{sequence:D4}";
    }

    /// <summary>
    /// 消費データを作成する
    /// </summary>
    public async Task<Consumption> CreateConsumptionAsync(ConsumptionCommand command)
    {
        // 入荷データの取得
        var receiving = await _receivingRepository.FindByReceivingNumberAsync(command.ReceivingNumber)
            ?? throw new InvalidOperationException($"Receiving not found: {command.ReceivingNumber}");

        // 関連する支給データを取得
        var supplies = await _supplyRepository.FindByPurchaseOrderDetailAsync(
            receiving.PurchaseOrderNumber, receiving.LineNumber);
        if (supplies.Count == 0)
        {
            throw new InvalidOperationException($"Supply not found for receiving: {command.ReceivingNumber}");
        }

        // 最初の支給を使用（通常は1つ）
        var supply = supplies[0];
        var supplyDetails = await _supplyDetailRepository.FindBySupplyNumberAsync(supply.SupplyNumber);

        // 消費数量のバリデーション
        foreach (var detailCommand in command.Details)
        {
            var supplyDetail = supplyDetails.FirstOrDefault(d => d.ItemCode == detailCommand.ItemCode);
            if (supplyDetail != null && detailCommand.Quantity > supplyDetail.Quantity)
            {
                throw new InvalidOperationException(
                    $"Consumption quantity ({detailCommand.Quantity}) exceeds supply quantity ({supplyDetail.Quantity})");
            }
        }

        // 消費番号を生成
        var consumptionNumber = await GenerateConsumptionNumberAsync(command.ConsumptionDate);

        // 消費データを作成
        var consumption = new Consumption
        {
            ConsumptionNumber = consumptionNumber,
            ReceivingNumber = command.ReceivingNumber,
            ConsumptionDate = command.ConsumptionDate,
            SupplierCode = command.SupplierCode,
            Remarks = command.Remarks,
            CreatedBy = command.CreatedBy
        };
        await _consumptionRepository.SaveAsync(consumption);

        // 消費明細を作成
        var details = new List<ConsumptionDetail>();
        var lineNumber = 0;

        foreach (var detailCommand in command.Details)
        {
            lineNumber++;

            var detail = new ConsumptionDetail
            {
                ConsumptionNumber = consumptionNumber,
                LineNumber = lineNumber,
                ItemCode = detailCommand.ItemCode,
                Quantity = detailCommand.Quantity,
                Remarks = detailCommand.Remarks
            };
            await _consumptionDetailRepository.SaveAsync(detail);

            details.Add(detail);
        }

        consumption.Details = details;
        return consumption;
    }

    /// <summary>
    /// 消費番号で検索する
    /// </summary>
    public async Task<Consumption?> FindByConsumptionNumberAsync(string consumptionNumber)
    {
        var consumption = await _consumptionRepository.FindByConsumptionNumberAsync(consumptionNumber);
        if (consumption != null)
        {
            consumption.Details = await _consumptionDetailRepository.FindByConsumptionNumberAsync(consumptionNumber);
        }
        return consumption;
    }

    /// <summary>
    /// 消費率を計算する
    /// </summary>
    public async Task<decimal> CalculateConsumptionRateAsync(string supplyNumber, string itemCode)
    {
        var supply = await _supplyRepository.FindBySupplyNumberAsync(supplyNumber)
            ?? throw new InvalidOperationException($"Supply not found: {supplyNumber}");

        var supplyDetails = await _supplyDetailRepository.FindBySupplyNumberAsync(supplyNumber);
        var targetDetail = supplyDetails.FirstOrDefault(d => d.ItemCode == itemCode)
            ?? throw new InvalidOperationException($"Supply detail not found: {itemCode}");

        var supplyQuantity = targetDetail.Quantity;

        var totalConsumption = await _consumptionDetailRepository.SumByPurchaseOrderAndItemAsync(
            supply.PurchaseOrderNumber,
            supply.LineNumber,
            itemCode);

        return supplyQuantity > 0
            ? Math.Round(totalConsumption / supplyQuantity, 2, MidpointRounding.AwayFromZero)
            : 0m;
    }
}
