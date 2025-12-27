using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Port.Out.Dto;
using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 外注委託ワークフローサービス
/// </summary>
public class SubcontractingWorkflowService
{
    private readonly IPurchaseOrderRepository _purchaseOrderRepository;
    private readonly IPurchaseOrderDetailRepository _purchaseOrderDetailRepository;
    private readonly ISupplyRepository _supplyRepository;
    private readonly ISupplyDetailRepository _supplyDetailRepository;
    private readonly IConsumptionDetailRepository _consumptionDetailRepository;
    private readonly IUnitPriceRepository _unitPriceRepository;

    public SubcontractingWorkflowService(
        IPurchaseOrderRepository purchaseOrderRepository,
        IPurchaseOrderDetailRepository purchaseOrderDetailRepository,
        ISupplyRepository supplyRepository,
        ISupplyDetailRepository supplyDetailRepository,
        IConsumptionDetailRepository consumptionDetailRepository,
        IUnitPriceRepository unitPriceRepository)
    {
        _purchaseOrderRepository = purchaseOrderRepository;
        _purchaseOrderDetailRepository = purchaseOrderDetailRepository;
        _supplyRepository = supplyRepository;
        _supplyDetailRepository = supplyDetailRepository;
        _consumptionDetailRepository = consumptionDetailRepository;
        _unitPriceRepository = unitPriceRepository;
    }

    /// <summary>
    /// 発注番号を生成する
    /// </summary>
    private async Task<string> GeneratePurchaseOrderNumberAsync(DateOnly orderDate)
    {
        var prefix = $"PO-{orderDate:yyyyMM}-";
        var latestNumber = await _purchaseOrderRepository.FindLatestPurchaseOrderNumberAsync($"{prefix}%");

        var sequence = 1;
        if (latestNumber != null)
        {
            var currentSequence = int.Parse(latestNumber[^4..]);
            sequence = currentSequence + 1;
        }

        return $"{prefix}{sequence:D4}";
    }

    /// <summary>
    /// 外注発注を作成する
    /// </summary>
    public async Task<PurchaseOrder> CreateSubcontractOrderAsync(SubcontractOrderCommand command)
    {
        var orderDate = DateOnly.FromDateTime(DateTime.Today);
        var purchaseOrderNumber = await GeneratePurchaseOrderNumberAsync(orderDate);

        // 単価を取得
        var unitPrice = command.UnitPrice;
        if (unitPrice == null)
        {
            var priceRecord = await _unitPriceRepository.FindEffectiveUnitPriceAsync(
                command.ItemCode, command.SupplierCode, orderDate);
            unitPrice = priceRecord?.Price ?? 0m;
        }

        // 発注ヘッダを作成
        var purchaseOrder = new PurchaseOrder
        {
            PurchaseOrderNumber = purchaseOrderNumber,
            OrderDate = orderDate,
            SupplierCode = command.SupplierCode,
            Status = PurchaseOrderStatus.Creating
        };
        await _purchaseOrderRepository.SaveAsync(purchaseOrder);

        // 発注明細を作成
        var orderAmount = command.Quantity * unitPrice.Value;
        var taxAmount = orderAmount * 0.10m; // 消費税10%

        var detail = new PurchaseOrderDetail
        {
            PurchaseOrderNumber = purchaseOrderNumber,
            LineNumber = 1,
            ItemCode = command.ItemCode,
            OrderQuantity = command.Quantity,
            OrderUnitPrice = unitPrice.Value,
            OrderAmount = orderAmount,
            TaxAmount = taxAmount,
            ExpectedReceivingDate = command.DeliveryDate
        };
        await _purchaseOrderDetailRepository.SaveAsync(detail);

        // 発注確定
        await _purchaseOrderRepository.UpdateStatusAsync(purchaseOrderNumber, PurchaseOrderStatus.Ordered);
        purchaseOrder.Status = PurchaseOrderStatus.Ordered;

        return purchaseOrder;
    }

    /// <summary>
    /// 外注委託状況を取得する
    /// </summary>
    public async Task<SubcontractStatus> GetSubcontractStatusAsync(string purchaseOrderNumber)
    {
        var purchaseOrder = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(purchaseOrderNumber)
            ?? throw new InvalidOperationException($"Purchase order not found: {purchaseOrderNumber}");

        var details = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAsync(purchaseOrderNumber);

        var suppliedQuantity = 0m;
        var consumedQuantity = 0m;
        var acceptedQuantity = 0m;

        foreach (var detail in details)
        {
            var supplies = await _supplyRepository.FindByPurchaseOrderDetailAsync(purchaseOrderNumber, detail.LineNumber);
            foreach (var supply in supplies)
            {
                var supplyDetails = await _supplyDetailRepository.FindBySupplyNumberAsync(supply.SupplyNumber);
                foreach (var supplyDetail in supplyDetails)
                {
                    suppliedQuantity += supplyDetail.Quantity;

                    var consumed = await _consumptionDetailRepository.SumByPurchaseOrderAndItemAsync(
                        purchaseOrderNumber, detail.LineNumber, supplyDetail.ItemCode);
                    consumedQuantity += consumed;
                }
            }

            acceptedQuantity += detail.AcceptedQuantity;
        }

        var yieldRate = suppliedQuantity > 0
            ? Math.Round(consumedQuantity / suppliedQuantity, 2, MidpointRounding.AwayFromZero)
            : 0m;

        return new SubcontractStatus
        {
            PurchaseOrderNumber = purchaseOrderNumber,
            Status = purchaseOrder.Status,
            SuppliedQuantity = suppliedQuantity,
            ConsumedQuantity = consumedQuantity,
            AcceptedQuantity = acceptedQuantity,
            YieldRate = yieldRate
        };
    }
}
