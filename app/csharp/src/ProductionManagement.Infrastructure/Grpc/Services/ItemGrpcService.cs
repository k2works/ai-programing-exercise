using Grpc.Core;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Grpc.Protos;
using ProductionManagement.Infrastructure.Grpc.Mappers;

namespace ProductionManagement.Infrastructure.Grpc.Services;

/// <summary>
/// 品目 gRPC サービス
/// </summary>
public class ItemGrpcService : ItemService.ItemServiceBase
{
    private readonly IItemUseCase _itemUseCase;
    private readonly ProtoMapper _mapper;

    public ItemGrpcService(IItemUseCase itemUseCase, ProtoMapper mapper)
    {
        _itemUseCase = itemUseCase;
        _mapper = mapper;
    }

    /// <summary>
    /// 品目取得（Unary）
    /// </summary>
    public override async Task<ItemMessage> GetItem(
        GetItemRequest request,
        ServerCallContext context)
    {
        var item = await _itemUseCase.GetItemByCodeAsync(request.ItemCode);
        return _mapper.ToProto(item);
    }

    /// <summary>
    /// 品目登録（Unary）
    /// </summary>
    public override async Task<ItemMessage> CreateItem(
        CreateItemRequest request,
        ServerCallContext context)
    {
        var command = _mapper.ToCreateCommand(request);
        var item = await _itemUseCase.CreateItemAsync(command);
        return _mapper.ToProto(item);
    }

    /// <summary>
    /// 品目更新（Unary）
    /// </summary>
    public override async Task<ItemMessage> UpdateItem(
        UpdateItemRequest request,
        ServerCallContext context)
    {
        var command = _mapper.ToUpdateCommand(request);
        var item = await _itemUseCase.UpdateItemAsync(command);
        return _mapper.ToProto(item);
    }

    /// <summary>
    /// 品目削除（Unary）
    /// </summary>
    public override async Task<Empty> DeleteItem(
        DeleteItemRequest request,
        ServerCallContext context)
    {
        await _itemUseCase.DeleteItemAsync(request.ItemCode);
        return new Empty();
    }

    /// <summary>
    /// 品目検索（Unary）
    /// </summary>
    public override async Task<GetItemsResponse> SearchItems(
        SearchItemsRequest request,
        ServerCallContext context)
    {
        var items = await _itemUseCase.SearchItemsAsync(request.Keyword);
        var response = new GetItemsResponse();
        response.Items.AddRange(items.Select(i => _mapper.ToProto(i)));
        return response;
    }

    /// <summary>
    /// 品目一覧ストリーム配信（Server Streaming）
    /// </summary>
    public override async Task StreamItems(
        GetItemsRequest request,
        IServerStreamWriter<ItemMessage> responseStream,
        ServerCallContext context)
    {
        var items = request.Category == ItemCategory.Unspecified
            ? await _itemUseCase.GetAllItemsAsync()
            : await _itemUseCase.GetItemsByCategoryAsync(_mapper.ToDomainCategory(request.Category));

        foreach (var item in items)
        {
            if (context.CancellationToken.IsCancellationRequested)
                break;

            await responseStream.WriteAsync(_mapper.ToProto(item));
        }
    }

    /// <summary>
    /// バッチ登録（Client Streaming）
    /// </summary>
    public override async Task<BatchCreateResponse> BatchCreateItems(
        IAsyncStreamReader<CreateItemRequest> requestStream,
        ServerCallContext context)
    {
        var successCount = 0;
        var failedCodes = new List<string>();

        await foreach (var request in requestStream.ReadAllAsync(context.CancellationToken))
        {
            try
            {
                var command = _mapper.ToCreateCommand(request);
                await _itemUseCase.CreateItemAsync(command);
                successCount++;
            }
            catch (Exception)
            {
                failedCodes.Add(request.ItemCode);
            }
        }

        var response = new BatchCreateResponse
        {
            SuccessCount = successCount,
            FailedCount = failedCodes.Count
        };
        response.FailedCodes.AddRange(failedCodes);

        return response;
    }
}
