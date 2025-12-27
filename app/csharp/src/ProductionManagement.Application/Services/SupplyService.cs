using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Subcontract;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 支給サービス
/// </summary>
public class SupplyService
{
    private readonly ISupplyRepository _supplyRepository;
    private readonly ISupplyDetailRepository _supplyDetailRepository;

    public SupplyService(
        ISupplyRepository supplyRepository,
        ISupplyDetailRepository supplyDetailRepository)
    {
        _supplyRepository = supplyRepository;
        _supplyDetailRepository = supplyDetailRepository;
    }

    /// <summary>
    /// 支給番号を生成する
    /// </summary>
    private async Task<string> GenerateSupplyNumberAsync(DateOnly supplyDate)
    {
        var prefix = $"SUP-{supplyDate:yyyyMM}-";
        var latestNumber = await _supplyRepository.FindLatestSupplyNumberAsync($"{prefix}%");

        var sequence = 1;
        if (latestNumber != null)
        {
            var currentSequence = int.Parse(latestNumber[^4..]);
            sequence = currentSequence + 1;
        }

        return $"{prefix}{sequence:D4}";
    }

    /// <summary>
    /// 支給データを作成する
    /// </summary>
    public async Task<Supply> CreateSupplyAsync(SupplyCommand command)
    {
        // 支給番号を生成
        var supplyNumber = await GenerateSupplyNumberAsync(command.SupplyDate);

        // 支給データを作成
        var supply = new Supply
        {
            SupplyNumber = supplyNumber,
            PurchaseOrderNumber = command.PurchaseOrderNumber,
            LineNumber = command.LineNumber,
            SupplierCode = command.SupplierCode,
            SupplyDate = command.SupplyDate,
            SupplierPersonCode = command.SupplierPersonCode,
            SupplyType = command.SupplyType,
            Remarks = command.Remarks,
            CreatedBy = command.CreatedBy
        };
        await _supplyRepository.SaveAsync(supply);

        // 支給明細を作成
        var details = new List<SupplyDetail>();
        var lineNumber = 0;

        foreach (var detailCommand in command.Details)
        {
            lineNumber++;

            var detail = new SupplyDetail
            {
                SupplyNumber = supplyNumber,
                LineNumber = lineNumber,
                ItemCode = detailCommand.ItemCode,
                Quantity = detailCommand.Quantity,
                UnitPrice = detailCommand.UnitPrice,
                Amount = detailCommand.Quantity * detailCommand.UnitPrice,
                Remarks = detailCommand.Remarks
            };
            await _supplyDetailRepository.SaveAsync(detail);

            details.Add(detail);
        }

        supply.Details = details;
        return supply;
    }

    /// <summary>
    /// 支給番号で検索する
    /// </summary>
    public async Task<Supply?> FindBySupplyNumberAsync(string supplyNumber)
    {
        var supply = await _supplyRepository.FindBySupplyNumberAsync(supplyNumber);
        if (supply != null)
        {
            supply.Details = await _supplyDetailRepository.FindBySupplyNumberAsync(supplyNumber);
        }
        return supply;
    }

    /// <summary>
    /// 発注明細で検索する
    /// </summary>
    public async Task<IReadOnlyList<Supply>> FindByPurchaseOrderDetailAsync(
        string purchaseOrderNumber, int lineNumber)
    {
        var supplies = await _supplyRepository.FindByPurchaseOrderDetailAsync(purchaseOrderNumber, lineNumber);
        foreach (var supply in supplies)
        {
            supply.Details = await _supplyDetailRepository.FindBySupplyNumberAsync(supply.SupplyNumber);
        }
        return supplies;
    }
}
