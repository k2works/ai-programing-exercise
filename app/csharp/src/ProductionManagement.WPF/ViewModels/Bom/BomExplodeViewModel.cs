using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.WPF.Services;

namespace ProductionManagement.WPF.ViewModels.Bom;

/// <summary>
/// BOM 展開 ViewModel
/// </summary>
public partial class BomExplodeViewModel : ObservableObject
{
    private readonly BomService _bomService;
    private readonly IItemUseCase _itemUseCase;
    private readonly INavigationService _navigationService;
    private readonly IDialogService _dialogService;

    public BomExplodeViewModel(
        BomService bomService,
        IItemUseCase itemUseCase,
        INavigationService navigationService,
        IDialogService dialogService)
    {
        _bomService = bomService;
        _itemUseCase = itemUseCase;
        _navigationService = navigationService;
        _dialogService = dialogService;

        // パラメータがあれば直接読み込み、なければ品目一覧を読み込み
        if (navigationService is NavigationService nav && nav.CurrentParameter is string itemCode)
        {
            _ = LoadAsync(itemCode);
        }
        else
        {
            _ = LoadItemsAsync();
        }
    }

    [ObservableProperty]
    private ObservableCollection<Item> _items = [];

    [ObservableProperty]
    private Item? _selectedItem;

    [ObservableProperty]
    private string _searchKeyword = string.Empty;

    [ObservableProperty]
    private Item? _rootItem;

    [ObservableProperty]
    private BomNode? _bomTree;

    [ObservableProperty]
    private bool _isLoading;

    [ObservableProperty]
    private bool _hasBomData;

    [ObservableProperty]
    private int _expandLevel = 99;

    partial void OnSelectedItemChanged(Item? value)
    {
        if (value != null)
        {
            _ = LoadAsync(value.ItemCode);
        }
        else
        {
            RootItem = null;
            BomTree = null;
            HasBomData = false;
        }
    }

    /// <summary>
    /// 品目一覧を読み込み（製品・半製品のみ）
    /// </summary>
    private async Task LoadItemsAsync()
    {
        try
        {
            IsLoading = true;

            var allItems = await _itemUseCase.GetAllItemsAsync();

            // 製品・半製品のみフィルタ（BOMを持つ可能性のある品目）
            var filteredItems = allItems
                .Where(i => i.ItemCategory == ItemCategory.Product ||
                           i.ItemCategory == ItemCategory.SemiProduct)
                .OrderBy(i => i.ItemCode)
                .ToList();

            Items = new ObservableCollection<Item>(filteredItems);
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// キーワード検索
    /// </summary>
    [RelayCommand]
    private async Task SearchAsync()
    {
        try
        {
            IsLoading = true;

            IReadOnlyList<Item> items;
            if (string.IsNullOrWhiteSpace(SearchKeyword))
            {
                items = await _itemUseCase.GetAllItemsAsync();
            }
            else
            {
                items = await _itemUseCase.SearchItemsAsync(SearchKeyword);
            }

            // 製品・半製品のみフィルタ
            var filteredItems = items
                .Where(i => i.ItemCategory == ItemCategory.Product ||
                           i.ItemCategory == ItemCategory.SemiProduct)
                .OrderBy(i => i.ItemCode)
                .ToList();

            Items = new ObservableCollection<Item>(filteredItems);
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// BOM 展開データ読み込み
    /// </summary>
    public async Task LoadAsync(string itemCode)
    {
        try
        {
            IsLoading = true;

            RootItem = await _itemUseCase.GetItemByCodeAsync(itemCode);
            BomTree = await _bomService.ExplodeBomAsync(itemCode);
            HasBomData = BomTree?.Children.Count > 0;

            // 選択状態を同期
            if (SelectedItem?.ItemCode != itemCode)
            {
                SelectedItem = Items.FirstOrDefault(i => i.ItemCode == itemCode);
            }
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
            HasBomData = false;
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// 戻る
    /// </summary>
    [RelayCommand]
    private void GoBack()
    {
        _navigationService.GoBack();
    }

    /// <summary>
    /// 品目詳細へ遷移
    /// </summary>
    [RelayCommand]
    private void ShowItemDetail(string itemCode)
    {
        _navigationService.NavigateTo("ItemDetail", itemCode);
    }

    /// <summary>
    /// 全展開
    /// </summary>
    [RelayCommand]
    private void ExpandAll()
    {
        ExpandLevel = 99;
    }

    /// <summary>
    /// 全折りたたみ
    /// </summary>
    [RelayCommand]
    private void CollapseAll()
    {
        ExpandLevel = 0;
    }
}
