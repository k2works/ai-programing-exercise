using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 発注サービス
/// </summary>
public class PurchaseOrderService : IPurchaseOrderUseCase
{
    private readonly IPurchaseOrderRepository _purchaseOrderRepository;
    private readonly IPurchaseOrderDetailRepository _purchaseOrderDetailRepository;
    private readonly IUnitPriceRepository _unitPriceRepository;

    public PurchaseOrderService(
        IPurchaseOrderRepository purchaseOrderRepository,
        IPurchaseOrderDetailRepository purchaseOrderDetailRepository,
        IUnitPriceRepository unitPriceRepository)
    {
        _purchaseOrderRepository = purchaseOrderRepository;
        _purchaseOrderDetailRepository = purchaseOrderDetailRepository;
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
    /// 発注を作成する
    /// </summary>
    public async Task<PurchaseOrder> CreateOrderAsync(PurchaseOrderCreateCommand input)
    {
        var purchaseOrderNumber = await GeneratePurchaseOrderNumberAsync(input.OrderDate);
        var taxRate = input.TaxRate ?? 10m;

        // 発注ヘッダを作成
        var purchaseOrder = new PurchaseOrder
        {
            PurchaseOrderNumber = purchaseOrderNumber,
            OrderDate = input.OrderDate,
            SupplierCode = input.SupplierCode,
            OrdererCode = input.OrdererCode,
            DepartmentCode = input.DepartmentCode,
            Status = PurchaseOrderStatus.Creating,
            Remarks = input.Remarks
        };
        await _purchaseOrderRepository.SaveAsync(purchaseOrder);

        // 発注明細を作成
        var details = new List<PurchaseOrderDetail>();
        var lineNumber = 0;

        foreach (var detailInput in input.Details)
        {
            lineNumber++;

            // 単価を取得
            var unitPrice = await _unitPriceRepository.FindEffectiveUnitPriceAsync(
                detailInput.ItemCode,
                input.SupplierCode,
                input.OrderDate);

            if (unitPrice == null)
            {
                throw new InvalidOperationException(
                    $"Unit price not found: {detailInput.ItemCode} / {input.SupplierCode}");
            }

            // 金額計算
            var orderAmount = unitPrice.Price * detailInput.OrderQuantity;
            var taxAmount = Math.Round(orderAmount * taxRate / 100m, MidpointRounding.AwayFromZero);

            var detail = new PurchaseOrderDetail
            {
                PurchaseOrderNumber = purchaseOrderNumber,
                LineNumber = lineNumber,
                OrderNumber = detailInput.OrderNumber,
                DeliveryLocationCode = detailInput.DeliveryLocationCode,
                ItemCode = detailInput.ItemCode,
                MiscellaneousItemFlag = false,
                ExpectedReceivingDate = detailInput.ExpectedReceivingDate,
                OrderUnitPrice = unitPrice.Price,
                OrderQuantity = detailInput.OrderQuantity,
                OrderAmount = orderAmount,
                TaxAmount = taxAmount
            };
            await _purchaseOrderDetailRepository.SaveAsync(detail);

            details.Add(detail);
        }

        purchaseOrder.Details = details;
        return purchaseOrder;
    }

    /// <summary>
    /// 発注を取得する
    /// </summary>
    public async Task<PurchaseOrder> GetOrderAsync(string purchaseOrderNumber)
    {
        var purchaseOrder = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(purchaseOrderNumber)
            ?? throw new PurchaseOrderNotFoundException(purchaseOrderNumber);

        var details = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAsync(purchaseOrderNumber);
        purchaseOrder.Details = details;
        return purchaseOrder;
    }

    /// <summary>
    /// 発注一覧を取得する
    /// </summary>
    public async Task<IReadOnlyList<PurchaseOrder>> GetAllOrdersAsync()
    {
        return await _purchaseOrderRepository.FindAllAsync();
    }

    /// <summary>
    /// 発注を確定する
    /// </summary>
    public async Task<PurchaseOrder> ConfirmOrderAsync(string purchaseOrderNumber)
    {
        var purchaseOrder = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(purchaseOrderNumber)
            ?? throw new PurchaseOrderNotFoundException(purchaseOrderNumber);

        if (purchaseOrder.Status != PurchaseOrderStatus.Creating)
        {
            throw new InvalidOperationException(
                $"Only creating purchase orders can be confirmed: {purchaseOrderNumber}");
        }

        await _purchaseOrderRepository.UpdateStatusAsync(purchaseOrderNumber, PurchaseOrderStatus.Ordered);
        purchaseOrder.Status = PurchaseOrderStatus.Ordered;
        return purchaseOrder;
    }

    /// <summary>
    /// 発注を取消する
    /// </summary>
    public async Task CancelOrderAsync(string purchaseOrderNumber)
    {
        var purchaseOrder = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(purchaseOrderNumber)
            ?? throw new PurchaseOrderNotFoundException(purchaseOrderNumber);

        var details = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAsync(purchaseOrderNumber);
        var hasReceived = details.Any(d => d.ReceivedQuantity > 0);

        if (hasReceived)
        {
            throw new InvalidOperationException(
                $"Cannot cancel purchase order with received items: {purchaseOrderNumber}");
        }

        await _purchaseOrderRepository.UpdateStatusAsync(purchaseOrderNumber, PurchaseOrderStatus.Cancelled);
    }
}
