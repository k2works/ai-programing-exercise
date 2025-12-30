using Grpc.Core;
using ProductionManagement.Grpc.Protos;
using ProductionManagement.Infrastructure.Grpc.Mappers;
using AppBomService = ProductionManagement.Application.Services.BomService;
using AppBomNode = ProductionManagement.Application.Services.BomNode;

namespace ProductionManagement.Infrastructure.Grpc.Services;

/// <summary>
/// BOM gRPC サービス
/// </summary>
public class BomGrpcService : BomService.BomServiceBase
{
    private readonly AppBomService _bomService;
    private readonly ProtoMapper _mapper;

    public BomGrpcService(AppBomService bomService, ProtoMapper mapper)
    {
        _bomService = bomService;
        _mapper = mapper;
    }

    /// <summary>
    /// 部品展開（Server Streaming）
    /// BOM を再帰的に展開し、各ノードをストリームで返す
    /// </summary>
    public override async Task ExplodeBom(
        ExplodeBomRequest request,
        IServerStreamWriter<BomNodeMessage> responseStream,
        ServerCallContext context)
    {
        var rootNode = await _bomService.ExplodeBomAsync(request.ItemCode);
        await StreamBomNodeAsync(rootNode, responseStream, context.CancellationToken);
    }

    /// <summary>
    /// BOM ノードを再帰的にストリーム送信
    /// </summary>
    private async Task StreamBomNodeAsync(
        AppBomNode node,
        IServerStreamWriter<BomNodeMessage> responseStream,
        CancellationToken cancellationToken)
    {
        if (cancellationToken.IsCancellationRequested)
            return;

        await responseStream.WriteAsync(_mapper.ToProto(node));

        if (node.Children is not null)
        {
            foreach (var child in node.Children)
            {
                await StreamBomNodeAsync(child, responseStream, cancellationToken);
            }
        }
    }

    /// <summary>
    /// 使用先照会（Unary）
    /// </summary>
    public override async Task<WhereUsedResponse> WhereUsed(
        WhereUsedRequest request,
        ServerCallContext context)
    {
        var results = await _bomService.WhereUsedAsync(request.ItemCode);

        var response = new WhereUsedResponse();
        response.Results.AddRange(results.Select(r => _mapper.ToProto(r)));

        return response;
    }
}
