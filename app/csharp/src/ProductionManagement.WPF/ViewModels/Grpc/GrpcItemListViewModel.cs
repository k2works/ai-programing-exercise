using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using Grpc.Core;
using ProductionManagement.Grpc.Protos;
using ProductionManagement.WPF.Services;
using ItemServiceClient = ProductionManagement.Grpc.Protos.ItemService.ItemServiceClient;

namespace ProductionManagement.WPF.ViewModels.Grpc;

/// <summary>
/// gRPC 品目一覧 ViewModel
/// </summary>
public partial class GrpcItemListViewModel : ViewModelBase
{
    private readonly IGrpcChannelFactory _channelFactory;
    private readonly ItemServiceClient _client;

    public GrpcItemListViewModel(
        IGrpcChannelFactory channelFactory,
        IDialogService dialogService) : base(dialogService)
    {
        _channelFactory = channelFactory;
        _client = new ItemServiceClient(_channelFactory.Channel);
        _ = LoadAsync();
    }

    /// <summary>
    /// 品目一覧
    /// </summary>
    public ObservableCollection<ItemMessage> Items { get; } = [];

    /// <summary>
    /// 選択中の品目
    /// </summary>
    [ObservableProperty]
    private ItemMessage? _selectedItem;

    /// <summary>
    /// 検索キーワード
    /// </summary>
    [ObservableProperty]
    private string _keyword = string.Empty;

    /// <summary>
    /// 選択中のカテゴリ
    /// </summary>
    [ObservableProperty]
    private ItemCategory _selectedCategory = ItemCategory.Unspecified;

    /// <summary>
    /// ストリーミング状態メッセージ
    /// </summary>
    [ObservableProperty]
    private string _streamingStatus = string.Empty;

    /// <summary>
    /// 受信アイテム数
    /// </summary>
    [ObservableProperty]
    private int _receivedCount;

    /// <summary>
    /// カテゴリ一覧
    /// </summary>
    public IReadOnlyList<ItemCategory> Categories { get; } = Enum.GetValues<ItemCategory>();

    /// <summary>
    /// データ読み込み（Server Streaming）
    /// </summary>
    [RelayCommand]
    private async Task LoadAsync()
    {
        await ExecuteAsync(async () =>
        {
            Items.Clear();
            ReceivedCount = 0;
            StreamingStatus = "ストリーミング受信中...";

            var request = new GetItemsRequest
            {
                Category = SelectedCategory
            };

            using var call = _client.StreamItems(request);

            await foreach (var item in call.ResponseStream.ReadAllAsync())
            {
                Items.Add(item);
                ReceivedCount = Items.Count;
                StreamingStatus = $"受信中: {ReceivedCount} 件";
            }

            StreamingStatus = $"完了: {ReceivedCount} 件受信";
        });
    }

    /// <summary>
    /// 検索実行（Unary RPC）
    /// </summary>
    [RelayCommand]
    private async Task SearchAsync()
    {
        if (string.IsNullOrWhiteSpace(Keyword))
        {
            await LoadAsync();
            return;
        }

        await ExecuteAsync(async () =>
        {
            Items.Clear();
            ReceivedCount = 0;
            StreamingStatus = "検索中...";

            var request = new SearchItemsRequest
            {
                Keyword = Keyword
            };

            var response = await _client.SearchItemsAsync(request);

            foreach (var item in response.Items)
            {
                Items.Add(item);
                ReceivedCount = Items.Count;
            }

            StreamingStatus = $"検索完了: {ReceivedCount} 件";
        });
    }

    /// <summary>
    /// 単一品目取得（Unary）
    /// </summary>
    [RelayCommand]
    private async Task GetItemAsync()
    {
        if (SelectedItem == null) return;

        await ExecuteAsync(async () =>
        {
            var request = new GetItemRequest { ItemCode = SelectedItem.ItemCode };
            var response = await _client.GetItemAsync(request);

            await DialogService.ShowInfoAsync(
                "品目詳細",
                $"品目コード: {response.ItemCode}\n" +
                $"品目名: {response.ItemName}\n" +
                $"カテゴリ: {response.Category}\n" +
                $"リードタイム: {response.LeadTime} 日\n" +
                $"安全在庫: {response.SafetyStock?.Value ?? "0"}");
        });
    }

    /// <summary>
    /// フィルタークリア
    /// </summary>
    [RelayCommand]
    private async Task ClearFilterAsync()
    {
        Keyword = string.Empty;
        SelectedCategory = ItemCategory.Unspecified;
        await LoadAsync();
    }

    partial void OnSelectedCategoryChanged(ItemCategory value)
    {
        _ = LoadAsync();
    }
}
