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

        if (navigationService is NavigationService nav && nav.CurrentParameter is string itemCode)
        {
            _ = LoadAsync(itemCode);
        }
    }

    [ObservableProperty]
    private Item? _rootItem;

    [ObservableProperty]
    private BomNode? _bomTree;

    [ObservableProperty]
    private bool _isLoading;

    [ObservableProperty]
    private int _expandLevel = 99;

    /// <summary>
    /// データ読み込み
    /// </summary>
    public async Task LoadAsync(string itemCode)
    {
        try
        {
            IsLoading = true;

            RootItem = await _itemUseCase.GetItemByCodeAsync(itemCode);
            BomTree = await _bomService.ExplodeBomAsync(itemCode);
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
            _navigationService.GoBack();
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
