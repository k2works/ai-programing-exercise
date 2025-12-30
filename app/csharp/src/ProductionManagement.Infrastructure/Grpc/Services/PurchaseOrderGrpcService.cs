using Grpc.Core;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Grpc.Protos;
using ProductionManagement.Infrastructure.Grpc.Mappers;

namespace ProductionManagement.Infrastructure.Grpc.Services;

/// <summary>
/// 発注 gRPC サービス
/// </summary>
public class PurchaseOrderGrpcService : ProductionManagement.Grpc.Protos.PurchaseOrderGrpcService.PurchaseOrderGrpcServiceBase
{
    private readonly IPurchaseOrderUseCase _purchaseOrderUseCase;
    private readonly ProtoMapper _mapper;

    public PurchaseOrderGrpcService(IPurchaseOrderUseCase purchaseOrderUseCase, ProtoMapper mapper)
    {
        _purchaseOrderUseCase = purchaseOrderUseCase;
        _mapper = mapper;
    }

    /// <summary>
    /// 発注取得（Unary）
    /// </summary>
    public override async Task<PurchaseOrderMessage> GetOrder(
        GetPurchaseOrderRequest request,
        ServerCallContext context)
    {
        var order = await _purchaseOrderUseCase.GetOrderAsync(request.PurchaseOrderNumber);
        return _mapper.ToProto(order);
    }

    /// <summary>
    /// 発注一覧ストリーム配信（Server Streaming）
    /// </summary>
    public override async Task StreamOrders(
        GetPurchaseOrdersRequest request,
        IServerStreamWriter<PurchaseOrderMessage> responseStream,
        ServerCallContext context)
    {
        var orders = await _purchaseOrderUseCase.GetAllOrdersAsync();

        // ステータスでフィルタリング
        if (request.Status != PurchaseOrderStatus.Unspecified)
        {
            var domainStatus = _mapper.ToDomainPurchaseOrderStatus(request.Status);
            orders = orders.Where(o => o.Status == domainStatus).ToList();
        }

        foreach (var order in orders)
        {
            if (context.CancellationToken.IsCancellationRequested)
                break;

            await responseStream.WriteAsync(_mapper.ToProto(order));
        }
    }

    /// <summary>
    /// 発注作成（Unary）
    /// </summary>
    public override async Task<PurchaseOrderMessage> CreateOrder(
        CreatePurchaseOrderRequest request,
        ServerCallContext context)
    {
        var command = _mapper.ToPurchaseOrderCreateCommand(request);
        var order = await _purchaseOrderUseCase.CreateOrderAsync(command);
        return _mapper.ToProto(order);
    }

    /// <summary>
    /// 発注確定（Unary）
    /// </summary>
    public override async Task<PurchaseOrderMessage> ConfirmOrder(
        ConfirmPurchaseOrderRequest request,
        ServerCallContext context)
    {
        var order = await _purchaseOrderUseCase.ConfirmOrderAsync(request.PurchaseOrderNumber);
        return _mapper.ToProto(order);
    }

    /// <summary>
    /// 発注取消（Unary）
    /// </summary>
    public override async Task<Empty> CancelOrder(
        CancelPurchaseOrderRequest request,
        ServerCallContext context)
    {
        await _purchaseOrderUseCase.CancelOrderAsync(request.PurchaseOrderNumber);
        return new Empty();
    }
}
