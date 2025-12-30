using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.WPF.Services;

namespace ProductionManagement.WPF.ViewModels.Items;

/// <summary>
/// 品目詳細 ViewModel
/// </summary>
public partial class ItemDetailViewModel : ObservableObject
{
    private readonly IItemUseCase _itemUseCase;
    private readonly BomService _bomService;
    private readonly INavigationService _navigationService;
    private readonly IDialogService _dialogService;

    public ItemDetailViewModel(
        IItemUseCase itemUseCase,
        BomService bomService,
        INavigationService navigationService,
        IDialogService dialogService)
    {
        _itemUseCase = itemUseCase;
        _bomService = bomService;
        _navigationService = navigationService;
        _dialogService = dialogService;

        if (navigationService is NavigationService nav && nav.CurrentParameter is string itemCode)
        {
            _ = LoadAsync(itemCode);
        }
    }

    [ObservableProperty]
    private Item? _item;

    [ObservableProperty]
    private IReadOnlyList<BomNode>? _bomChildren;

    [ObservableProperty]
    private IReadOnlyList<WhereUsedResult>? _whereUsedItems;

    [ObservableProperty]
    private bool _isLoading;

    /// <summary>
    /// データ読み込み
    /// </summary>
    public async Task LoadAsync(string itemCode)
    {
        try
        {
            IsLoading = true;

            Item = await _itemUseCase.GetItemByCodeAsync(itemCode);

            try
            {
                var bomTree = await _bomService.ExplodeBomAsync(itemCode);
                BomChildren = bomTree.Children;
            }
            catch
            {
                BomChildren = [];
            }

            try
            {
                WhereUsedItems = await _bomService.WhereUsedAsync(itemCode);
            }
            catch
            {
                WhereUsedItems = [];
            }
        }
        catch (ItemNotFoundException)
        {
            await _dialogService.ShowErrorAsync("エラー", "品目が見つかりません");
            _navigationService.GoBack();
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// 編集画面へ遷移
    /// </summary>
    [RelayCommand]
    private void Edit()
    {
        if (Item != null)
        {
            _navigationService.NavigateTo("ItemEdit", new ItemEditParameter(
                Mode: EditMode.Edit,
                ItemCode: Item.ItemCode
            ));
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
    /// BOM 展開画面へ遷移
    /// </summary>
    [RelayCommand]
    private void ShowBomExplode()
    {
        if (Item != null)
        {
            _navigationService.NavigateTo("BomExplode", Item.ItemCode);
        }
    }
}
