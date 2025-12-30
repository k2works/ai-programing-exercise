using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using Microsoft.Extensions.DependencyInjection;
using ProductionManagement.WPF.Services;
using ProductionManagement.WPF.ViewModels.Bom;
using ProductionManagement.WPF.ViewModels.Items;
using ProductionManagement.WPF.ViewModels.Suppliers;

namespace ProductionManagement.WPF.ViewModels;

/// <summary>
/// メインウィンドウ ViewModel
/// </summary>
public partial class MainViewModel : ObservableObject
{
    private readonly INavigationService _navigationService;
    private readonly IServiceProvider _serviceProvider;

    [ObservableProperty]
    private object? _currentView;

    [ObservableProperty]
    private string _statusMessage = "準備完了";

    [ObservableProperty]
    private bool _isLoading;

    [ObservableProperty]
    private string _currentUser = "admin";

    [ObservableProperty]
    private DateTime _currentDate = DateTime.Today;

    public MainViewModel(INavigationService navigationService, IServiceProvider serviceProvider)
    {
        _navigationService = navigationService;
        _serviceProvider = serviceProvider;
        _navigationService.NavigationRequested += OnNavigationRequested;
        _navigationService.NavigationWithParameterRequested += OnNavigationWithParameterRequested;

        NavigateToItemList();
    }

    public void SetLoading(bool isLoading, string? message = null)
    {
        IsLoading = isLoading;
        if (message != null)
        {
            StatusMessage = message;
        }
    }

    [RelayCommand]
    private void Navigate(string viewName)
    {
        _navigationService.NavigateTo(viewName);
    }

    [RelayCommand]
    private void NavigateToItemList()
    {
        _navigationService.NavigateTo("ItemList");
    }

    [RelayCommand]
    private void NavigateToSupplierList()
    {
        _navigationService.NavigateTo("SupplierList");
    }

    private void OnNavigationRequested(object? sender, string viewName)
    {
        CurrentView = CreateViewModel(viewName);
        StatusMessage = GetViewDisplayName(viewName);
    }

    private void OnNavigationWithParameterRequested(object? sender, (string ViewName, object Parameter) args)
    {
        CurrentView = CreateViewModel(args.ViewName);
        StatusMessage = GetViewDisplayName(args.ViewName);
    }

    private object? CreateViewModel(string viewName)
    {
        return viewName switch
        {
            "ItemList" => _serviceProvider.GetRequiredService<ItemListViewModel>(),
            "ItemDetail" => _serviceProvider.GetRequiredService<ItemDetailViewModel>(),
            "ItemEdit" => _serviceProvider.GetRequiredService<ItemEditViewModel>(),
            "BomExplode" => _serviceProvider.GetRequiredService<BomExplodeViewModel>(),
            "SupplierList" => _serviceProvider.GetRequiredService<SupplierListViewModel>(),
            "SupplierDetail" => _serviceProvider.GetRequiredService<SupplierDetailViewModel>(),
            "SupplierEdit" => _serviceProvider.GetRequiredService<SupplierEditViewModel>(),
            _ => null
        };
    }

    private static string GetViewDisplayName(string viewName)
    {
        return viewName switch
        {
            "ItemList" => "品目一覧",
            "ItemDetail" => "品目詳細",
            "ItemEdit" => "品目登録/編集",
            "BomExplode" => "BOM 展開",
            "SupplierList" => "取引先一覧",
            "SupplierDetail" => "取引先詳細",
            "SupplierEdit" => "取引先登録/編集",
            _ => viewName
        };
    }
}
