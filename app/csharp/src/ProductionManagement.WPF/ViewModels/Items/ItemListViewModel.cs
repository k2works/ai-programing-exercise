using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.WPF.Services;

namespace ProductionManagement.WPF.ViewModels.Items;

/// <summary>
/// 品目一覧 ViewModel
/// </summary>
public partial class ItemListViewModel : ObservableObject
{
    private readonly IItemUseCase _itemUseCase;
    private readonly INavigationService _navigationService;
    private readonly IDialogService _dialogService;

    public ItemListViewModel(
        IItemUseCase itemUseCase,
        INavigationService navigationService,
        IDialogService dialogService)
    {
        _itemUseCase = itemUseCase;
        _navigationService = navigationService;
        _dialogService = dialogService;

        _ = LoadAsync();
    }

    /// <summary>
    /// 品目一覧
    /// </summary>
    public ObservableCollection<Item> Items { get; } = [];

    /// <summary>
    /// 選択中の品目
    /// </summary>
    [ObservableProperty]
    [NotifyCanExecuteChangedFor(nameof(ShowDetailCommand))]
    [NotifyCanExecuteChangedFor(nameof(DeleteCommand))]
    private Item? _selectedItem;

    /// <summary>
    /// 検索キーワード
    /// </summary>
    [ObservableProperty]
    private string _keyword = string.Empty;

    /// <summary>
    /// 選択中の品目区分（フィルター用）
    /// </summary>
    [ObservableProperty]
    private ItemCategory? _selectedCategory;

    /// <summary>
    /// 読み込み中フラグ
    /// </summary>
    [ObservableProperty]
    private bool _isLoading;

    /// <summary>
    /// 品目区分リスト（コンボボックス用）
    /// </summary>
    public IReadOnlyList<ItemCategory> Categories { get; } = Enum.GetValues<ItemCategory>();

    /// <summary>
    /// データ読み込み
    /// </summary>
    [RelayCommand]
    private async Task LoadAsync()
    {
        try
        {
            IsLoading = true;
            Items.Clear();

            IReadOnlyList<Item> items;

            if (SelectedCategory.HasValue)
            {
                items = await _itemUseCase.GetItemsByCategoryAsync(SelectedCategory.Value);
            }
            else if (!string.IsNullOrWhiteSpace(Keyword))
            {
                items = await _itemUseCase.SearchItemsAsync(Keyword);
            }
            else
            {
                items = await _itemUseCase.GetAllItemsAsync();
            }

            foreach (var item in items)
            {
                Items.Add(item);
            }
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorDetailAsync(
                "データ読み込みエラー",
                "品目データの読み込みに失敗しました。データベース接続を確認してください。",
                ex.Message);
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// 検索実行
    /// </summary>
    [RelayCommand]
    private async Task SearchAsync()
    {
        await LoadAsync();
    }

    /// <summary>
    /// フィルタークリア
    /// </summary>
    [RelayCommand]
    private async Task ClearFilterAsync()
    {
        Keyword = string.Empty;
        SelectedCategory = null;
        await LoadAsync();
    }

    /// <summary>
    /// 詳細画面へ遷移
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanShowDetail))]
    private void ShowDetail()
    {
        if (SelectedItem != null)
        {
            _navigationService.NavigateTo("ItemDetail", SelectedItem.ItemCode);
        }
    }

    private bool CanShowDetail() => SelectedItem != null;

    /// <summary>
    /// 新規登録画面へ遷移
    /// </summary>
    [RelayCommand]
    private void Create()
    {
        _navigationService.NavigateTo("ItemEdit", new ItemEditParameter(Mode: EditMode.Create));
    }

    /// <summary>
    /// 品目削除
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanDelete))]
    private async Task DeleteAsync()
    {
        if (SelectedItem == null)
        {
            return;
        }

        var confirmed = await _dialogService.ShowConfirmAsync(
            "削除確認",
            $"品目「{SelectedItem.ItemCode}」を削除しますか？");

        if (!confirmed)
        {
            return;
        }

        try
        {
            await _itemUseCase.DeleteItemAsync(SelectedItem.ItemCode);
            await _dialogService.ShowInfoAsync("完了", "削除しました");
            await LoadAsync();
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
        }
    }

    private bool CanDelete() => SelectedItem != null;

    /// <summary>
    /// 行ダブルクリック
    /// </summary>
    [RelayCommand]
    private void RowDoubleClick()
    {
        ShowDetail();
    }

    partial void OnSelectedCategoryChanged(ItemCategory? value)
    {
        _ = LoadAsync();
    }
}
