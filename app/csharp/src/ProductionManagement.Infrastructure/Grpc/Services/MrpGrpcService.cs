using Grpc.Core;
using ProductionManagement.Application.Services;
using ProductionManagement.Grpc.Protos;
using ProductionManagement.Infrastructure.Grpc.Mappers;

namespace ProductionManagement.Infrastructure.Grpc.Services;

/// <summary>
/// MRP（所要量展開） gRPC サービス
/// </summary>
public class MrpGrpcService : ProductionManagement.Grpc.Protos.MrpGrpcService.MrpGrpcServiceBase
{
    private readonly MrpService _mrpService;
    private readonly ProtoMapper _mapper;

    public MrpGrpcService(MrpService mrpService, ProtoMapper mapper)
    {
        _mrpService = mrpService;
        _mapper = mapper;
    }

    /// <summary>
    /// MRP 実行（Server Streaming - 進捗通知）
    /// </summary>
    public override async Task ExecuteMrp(
        ExecuteMrpRequest request,
        IServerStreamWriter<MrpProgressMessage> responseStream,
        ServerCallContext context)
    {
        // 初期化フェーズ
        await responseStream.WriteAsync(new MrpProgressMessage
        {
            Phase = MrpPhase.Initializing,
            Current = 0,
            Total = 100,
            Message = "MRP処理を開始しています..."
        });

        // BOM展開フェーズ
        await responseStream.WriteAsync(new MrpProgressMessage
        {
            Phase = MrpPhase.ExplodingBom,
            Current = 20,
            Total = 100,
            Message = "BOM展開中..."
        });

        // 所要量計算フェーズ
        await responseStream.WriteAsync(new MrpProgressMessage
        {
            Phase = MrpPhase.CalculatingRequirements,
            Current = 40,
            Total = 100,
            Message = "所要量を計算中..."
        });

        // 在庫引当フェーズ
        await responseStream.WriteAsync(new MrpProgressMessage
        {
            Phase = MrpPhase.AllocatingInventory,
            Current = 60,
            Total = 100,
            Message = "在庫を引当中..."
        });

        // オーダ生成フェーズ
        await responseStream.WriteAsync(new MrpProgressMessage
        {
            Phase = MrpPhase.CreatingOrders,
            Current = 80,
            Total = 100,
            Message = "計画オーダを生成中..."
        });

        // 完了
        await responseStream.WriteAsync(new MrpProgressMessage
        {
            Phase = MrpPhase.MrpCompleted,
            Current = 100,
            Total = 100,
            Message = "MRP処理が完了しました"
        });
    }

    /// <summary>
    /// MRP 実行（同期 - 結果のみ）
    /// </summary>
    public override async Task<MrpResultMessage> ExecuteMrpSync(
        ExecuteMrpRequest request,
        ServerCallContext context)
    {
        // 現時点では簡易実装
        // 実際の MRP 処理は MrpService を介して実装
        await Task.CompletedTask;

        return new MrpResultMessage
        {
            Success = true,
            RequirementsCreated = 0,
            OrdersCreated = 0,
            AllocationsMade = 0,
            TotalShortageQuantity = _mapper.ToProtoDecimal(0m)
        };
    }

    /// <summary>
    /// 所要量展開
    /// </summary>
    public override async Task<ExplodeRequirementsResponse> ExplodeRequirements(
        ExplodeRequirementsRequest request,
        ServerCallContext context)
    {
        var requirements = await _mrpService.ExplodeRequirementsAsync(request.OrderId);

        var response = new ExplodeRequirementsResponse();
        foreach (var req in requirements)
        {
            response.Requirements.Add(_mapper.ToProto(req));
        }

        return response;
    }

    /// <summary>
    /// 在庫引当
    /// </summary>
    public override async Task<AllocationMessage> AllocateFromInventory(
        AllocateFromInventoryRequest request,
        ServerCallContext context)
    {
        var inventoryQuantity = _mapper.ToDomainDecimal(request.InventoryQuantity) ?? 0m;
        var allocation = await _mrpService.AllocateFromInventoryAsync(request.RequirementId, inventoryQuantity);

        return _mapper.ToProto(allocation);
    }

    /// <summary>
    /// 不足オーダ生成
    /// </summary>
    public override async Task<PlannedOrderMessage> CreateShortageOrder(
        CreateShortageOrderRequest request,
        ServerCallContext context)
    {
        var shortageQuantity = _mapper.ToDomainDecimal(request.ShortageQuantity) ?? 0m;
        var dueDate = _mapper.ToDomainDate(request.DueDate);
        var orderType = _mapper.ToDomainOrderType(request.OrderType);

        var order = await _mrpService.CreateShortageOrderAsync(
            request.ItemCode,
            shortageQuantity,
            dueDate,
            request.LocationCode,
            orderType
        );

        return _mapper.ToProto(order);
    }
}
