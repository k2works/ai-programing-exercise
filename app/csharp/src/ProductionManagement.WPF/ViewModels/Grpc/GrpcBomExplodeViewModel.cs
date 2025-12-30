using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using Grpc.Core;
using ProductionManagement.Grpc.Protos;
using ProductionManagement.WPF.Services;
using BomServiceClient = ProductionManagement.Grpc.Protos.BomService.BomServiceClient;
using ItemServiceClient = ProductionManagement.Grpc.Protos.ItemService.ItemServiceClient;

namespace ProductionManagement.WPF.ViewModels.Grpc;

/// <summary>
/// gRPC BOM 展開 ViewModel
/// </summary>
public partial class GrpcBomExplodeViewModel : ViewModelBase
{
    private readonly IGrpcChannelFactory _channelFactory;
    private readonly BomServiceClient _client;
    private readonly ItemServiceClient _itemClient;

    public GrpcBomExplodeViewModel(
        IGrpcChannelFactory channelFactory,
        IDialogService dialogService) : base(dialogService)
    {
        _channelFactory = channelFactory;
        _client = new BomServiceClient(_channelFactory.Channel);
        _itemClient = new ItemServiceClient(_channelFactory.Channel);
        _ = LoadItemsAsync();
    }

    /// <summary>
    /// 品目一覧（コンボボックス用）
    /// </summary>
    public ObservableCollection<ItemMessage> Items { get; } = [];

    /// <summary>
    /// 選択中の品目
    /// </summary>
    [ObservableProperty]
    [NotifyCanExecuteChangedFor(nameof(ExplodeCommand))]
    [NotifyCanExecuteChangedFor(nameof(WhereUsedCommand))]
    private ItemMessage? _selectedItem;

    /// <summary>
    /// BOM ツリー
    /// </summary>
    public ObservableCollection<BomNodeMessage> BomTree { get; } = [];

    /// <summary>
    /// 逆展開結果
    /// </summary>
    public ObservableCollection<WhereUsedResultMessage> WhereUsedResults { get; } = [];

    /// <summary>
    /// ストリーミング状態メッセージ
    /// </summary>
    [ObservableProperty]
    private string _streamingStatus = string.Empty;

    /// <summary>
    /// 品目一覧を読み込み
    /// </summary>
    private async Task LoadItemsAsync()
    {
        await ExecuteAsync(async () =>
        {
            Items.Clear();
            StreamingStatus = "品目読み込み中...";

            var request = new GetItemsRequest { Category = ItemCategory.Unspecified };
            using var call = _itemClient.StreamItems(request);

            await foreach (var item in call.ResponseStream.ReadAllAsync())
            {
                Items.Add(item);
            }

            StreamingStatus = $"品目を {Items.Count} 件読み込みました";
        });
    }

    /// <summary>
    /// BOM 展開（正展開 - Server Streaming）
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanExplode))]
    private async Task ExplodeAsync()
    {
        if (SelectedItem == null)
        {
            return;
        }

        await ExecuteAsync(async () =>
        {
            BomTree.Clear();
            WhereUsedResults.Clear();
            StreamingStatus = "BOM 展開中（ストリーミング）...";

            var request = new ExplodeBomRequest
            {
                ItemCode = SelectedItem.ItemCode
            };

            using var call = _client.ExplodeBom(request);

            await foreach (var node in call.ResponseStream.ReadAllAsync())
            {
                BomTree.Add(node);
                StreamingStatus = $"BOM 受信中: {BomTree.Count} ノード";
            }

            StreamingStatus = $"BOM 展開完了: {BomTree.Count} ノード";
        });
    }

    private bool CanExplode() => SelectedItem != null;

    /// <summary>
    /// 逆展開（Where-Used - Unary）
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanWhereUsed))]
    private async Task WhereUsedAsync()
    {
        if (SelectedItem == null)
        {
            return;
        }

        await ExecuteAsync(async () =>
        {
            BomTree.Clear();
            WhereUsedResults.Clear();
            StreamingStatus = "逆展開中...";

            var request = new WhereUsedRequest
            {
                ItemCode = SelectedItem.ItemCode
            };

            var response = await _client.WhereUsedAsync(request);

            foreach (var result in response.Results)
            {
                WhereUsedResults.Add(result);
            }

            StreamingStatus = $"逆展開完了: {WhereUsedResults.Count} 件";
        });
    }

    private bool CanWhereUsed() => SelectedItem != null;

    /// <summary>
    /// クリア
    /// </summary>
    [RelayCommand]
    private void Clear()
    {
        BomTree.Clear();
        WhereUsedResults.Clear();
        StreamingStatus = string.Empty;
    }
}
