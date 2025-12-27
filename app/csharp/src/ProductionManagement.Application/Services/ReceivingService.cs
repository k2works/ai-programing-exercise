using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 入荷・検収サービス
/// </summary>
public class ReceivingService
{
    private readonly IReceivingRepository _receivingRepository;
    private readonly IInspectionRepository _inspectionRepository;
    private readonly IAcceptanceRepository _acceptanceRepository;
    private readonly IPurchaseOrderRepository _purchaseOrderRepository;
    private readonly IPurchaseOrderDetailRepository _purchaseOrderDetailRepository;

    public ReceivingService(
        IReceivingRepository receivingRepository,
        IInspectionRepository inspectionRepository,
        IAcceptanceRepository acceptanceRepository,
        IPurchaseOrderRepository purchaseOrderRepository,
        IPurchaseOrderDetailRepository purchaseOrderDetailRepository)
    {
        _receivingRepository = receivingRepository;
        _inspectionRepository = inspectionRepository;
        _acceptanceRepository = acceptanceRepository;
        _purchaseOrderRepository = purchaseOrderRepository;
        _purchaseOrderDetailRepository = purchaseOrderDetailRepository;
    }

    /// <summary>
    /// 入荷番号を生成する
    /// </summary>
    private async Task<string> GenerateReceivingNumberAsync(DateOnly receivingDate)
    {
        var prefix = $"RCV-{receivingDate:yyyyMM}-";
        var latestNumber = await _receivingRepository.FindLatestReceivingNumberAsync($"{prefix}%");

        var sequence = 1;
        if (latestNumber != null)
        {
            var currentSequence = int.Parse(latestNumber[^4..]);
            sequence = currentSequence + 1;
        }

        return $"{prefix}{sequence:D4}";
    }

    /// <summary>
    /// 検査番号を生成する
    /// </summary>
    private async Task<string> GenerateInspectionNumberAsync(DateOnly inspectionDate)
    {
        var prefix = $"INS-{inspectionDate:yyyyMM}-";
        var latestNumber = await _inspectionRepository.FindLatestInspectionNumberAsync($"{prefix}%");

        var sequence = 1;
        if (latestNumber != null)
        {
            var currentSequence = int.Parse(latestNumber[^4..]);
            sequence = currentSequence + 1;
        }

        return $"{prefix}{sequence:D4}";
    }

    /// <summary>
    /// 検収番号を生成する
    /// </summary>
    private async Task<string> GenerateAcceptanceNumberAsync(DateOnly acceptanceDate)
    {
        var prefix = $"ACC-{acceptanceDate:yyyyMM}-";
        var latestNumber = await _acceptanceRepository.FindLatestAcceptanceNumberAsync($"{prefix}%");

        var sequence = 1;
        if (latestNumber != null)
        {
            var currentSequence = int.Parse(latestNumber[^4..]);
            sequence = currentSequence + 1;
        }

        return $"{prefix}{sequence:D4}";
    }

    /// <summary>
    /// 入荷を登録する
    /// </summary>
    public async Task<Receiving> RegisterReceivingAsync(ReceivingCommand command)
    {
        // 発注明細を取得
        var orderDetail = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAndLineNumberAsync(
            command.PurchaseOrderNumber, command.LineNumber);

        if (orderDetail == null)
        {
            throw new InvalidOperationException(
                $"Purchase order detail not found: {command.PurchaseOrderNumber}/{command.LineNumber}");
        }

        // 発注を取得
        var purchaseOrder = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(
            command.PurchaseOrderNumber);

        if (purchaseOrder == null)
        {
            throw new InvalidOperationException(
                $"Purchase order not found: {command.PurchaseOrderNumber}");
        }

        // 発注ステータスチェック
        if (purchaseOrder.Status != PurchaseOrderStatus.Ordered &&
            purchaseOrder.Status != PurchaseOrderStatus.PartiallyReceived)
        {
            throw new InvalidOperationException(
                $"Cannot receive: Purchase order status is {purchaseOrder.Status.GetDisplayName()}");
        }

        // 残数量チェック
        var remainingQuantity = orderDetail.OrderQuantity - orderDetail.ReceivedQuantity;
        if (command.ReceivingQuantity > remainingQuantity)
        {
            throw new InvalidOperationException(
                $"Receiving quantity ({command.ReceivingQuantity}) exceeds remaining quantity ({remainingQuantity})");
        }

        // 入荷番号を生成
        var receivingNumber = await GenerateReceivingNumberAsync(command.ReceivingDate);

        // 入荷データを作成
        var receiving = new Receiving
        {
            ReceivingNumber = receivingNumber,
            PurchaseOrderNumber = command.PurchaseOrderNumber,
            LineNumber = command.LineNumber,
            ReceivingDate = command.ReceivingDate,
            ItemCode = orderDetail.ItemCode,
            ReceivingQuantity = command.ReceivingQuantity,
            ReceivingType = command.ReceivingType,
            DeliveryNoteNumber = command.DeliveryNoteNumber,
            LotNumber = command.LotNumber,
            ReceivingLocationCode = command.ReceivingLocationCode,
            Remarks = command.Remarks,
            CreatedBy = command.CreatedBy
        };

        await _receivingRepository.SaveAsync(receiving);

        // 発注明細の入荷済数量を更新
        var newReceivedQuantity = orderDetail.ReceivedQuantity + command.ReceivingQuantity;
        await _purchaseOrderDetailRepository.UpdateReceivedQuantityAsync(
            command.PurchaseOrderNumber, command.LineNumber, newReceivedQuantity);

        // 発注ステータスを更新（更新後のデータを取得するため、ReceivedQuantityは既に更新済み）
        var allDetails = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAsync(
            command.PurchaseOrderNumber);
        var allReceived = allDetails.All(d => d.OrderQuantity <= d.ReceivedQuantity);

        var newStatus = allReceived ? PurchaseOrderStatus.Received : PurchaseOrderStatus.PartiallyReceived;
        await _purchaseOrderRepository.UpdateStatusAsync(command.PurchaseOrderNumber, newStatus);

        return receiving;
    }

    /// <summary>
    /// 受入検査を登録する
    /// </summary>
    public async Task<Inspection> RegisterInspectionAsync(InspectionCommand command)
    {
        // 入荷データを取得
        var receiving = await _receivingRepository.FindByReceivingNumberAsync(command.ReceivingNumber);

        if (receiving == null)
        {
            throw new InvalidOperationException(
                $"Receiving not found: {command.ReceivingNumber}");
        }

        // 検査数量チェック
        if (command.InspectionQuantity > receiving.ReceivingQuantity)
        {
            throw new InvalidOperationException(
                $"Inspection quantity ({command.InspectionQuantity}) exceeds receiving quantity ({receiving.ReceivingQuantity})");
        }

        // 合格数量 + 不合格数量 = 検査数量
        if (command.PassedQuantity + command.FailedQuantity != command.InspectionQuantity)
        {
            throw new InvalidOperationException(
                $"Passed quantity ({command.PassedQuantity}) + Failed quantity ({command.FailedQuantity}) must equal Inspection quantity ({command.InspectionQuantity})");
        }

        // 検査番号を生成
        var inspectionNumber = await GenerateInspectionNumberAsync(command.InspectionDate);

        // 検査データを作成
        var inspection = new Inspection
        {
            InspectionNumber = inspectionNumber,
            ReceivingNumber = command.ReceivingNumber,
            InspectionDate = command.InspectionDate,
            InspectionQuantity = command.InspectionQuantity,
            PassedQuantity = command.PassedQuantity,
            FailedQuantity = command.FailedQuantity,
            InspectionResult = command.InspectionResult,
            DefectCode = command.DefectCode,
            InspectorCode = command.InspectorCode,
            Remarks = command.Remarks,
            CreatedBy = command.CreatedBy
        };

        await _inspectionRepository.SaveAsync(inspection);

        // 発注明細の検査済数量を更新
        var orderDetail = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAndLineNumberAsync(
            receiving.PurchaseOrderNumber, receiving.LineNumber);

        if (orderDetail != null)
        {
            var newInspectedQuantity = orderDetail.InspectedQuantity + command.InspectionQuantity;
            await _purchaseOrderDetailRepository.UpdateInspectedQuantityAsync(
                receiving.PurchaseOrderNumber, receiving.LineNumber, newInspectedQuantity);
        }

        return inspection;
    }

    /// <summary>
    /// 検収を処理する
    /// </summary>
    public async Task<Acceptance> ProcessAcceptanceAsync(AcceptanceCommand command)
    {
        // 検査データを取得
        var inspection = await _inspectionRepository.FindByInspectionNumberAsync(command.InspectionNumber);

        if (inspection == null)
        {
            throw new InvalidOperationException(
                $"Inspection not found: {command.InspectionNumber}");
        }

        // 検収数量チェック
        if (command.AcceptanceQuantity > inspection.PassedQuantity)
        {
            throw new InvalidOperationException(
                $"Acceptance quantity ({command.AcceptanceQuantity}) exceeds passed quantity ({inspection.PassedQuantity})");
        }

        // 検収番号を生成
        var acceptanceNumber = await GenerateAcceptanceNumberAsync(command.AcceptanceDate);

        // 検収データを作成
        var acceptance = new Acceptance
        {
            AcceptanceNumber = acceptanceNumber,
            InspectionNumber = command.InspectionNumber,
            AcceptanceDate = command.AcceptanceDate,
            AcceptanceQuantity = command.AcceptanceQuantity,
            AcceptanceAmount = command.AcceptanceAmount,
            TaxAmount = command.TaxAmount,
            StorageLocationCode = command.StorageLocationCode,
            Remarks = command.Remarks,
            CreatedBy = command.CreatedBy
        };

        await _acceptanceRepository.SaveAsync(acceptance);

        // 入荷データから発注情報を取得
        var receiving = await _receivingRepository.FindByReceivingNumberAsync(inspection.ReceivingNumber);

        if (receiving != null)
        {
            // 発注明細の検収済数量を更新
            var orderDetail = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAndLineNumberAsync(
                receiving.PurchaseOrderNumber, receiving.LineNumber);

            if (orderDetail != null)
            {
                var newAcceptedQuantity = orderDetail.AcceptedQuantity + command.AcceptanceQuantity;
                await _purchaseOrderDetailRepository.UpdateAcceptedQuantityAsync(
                    receiving.PurchaseOrderNumber, receiving.LineNumber, newAcceptedQuantity);

                // 発注ステータスを更新（全明細が検収完了か確認、AcceptedQuantityは既に更新済み）
                var allDetails = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAsync(
                    receiving.PurchaseOrderNumber);
                var allAccepted = allDetails.All(d => d.OrderQuantity <= d.AcceptedQuantity);

                if (allAccepted)
                {
                    await _purchaseOrderRepository.UpdateStatusAsync(
                        receiving.PurchaseOrderNumber, PurchaseOrderStatus.Accepted);
                }
            }
        }

        return acceptance;
    }
}
