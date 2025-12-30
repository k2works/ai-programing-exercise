using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using Microsoft.Extensions.DependencyInjection;
using ProductionManagement.WPF.Services;
using ProductionManagement.WPF.ViewModels.Bom;
using ProductionManagement.WPF.ViewModels.Items;
using ProductionManagement.WPF.ViewModels.Planning;
using ProductionManagement.WPF.ViewModels.PurchaseOrders;
using ProductionManagement.WPF.ViewModels.Inventory;
using ProductionManagement.WPF.ViewModels.Suppliers;
using ProductionManagement.WPF.ViewModels.WorkOrders;
using ProductionManagement.WPF.ViewModels.Reports;

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

    [RelayCommand]
    private void NavigateToPurchaseOrderList()
    {
        _navigationService.NavigateTo("PurchaseOrderList");
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
            "PurchaseOrderList" => _serviceProvider.GetRequiredService<PurchaseOrderListViewModel>(),
            "PurchaseOrderEdit" => _serviceProvider.GetRequiredService<PurchaseOrderEditViewModel>(),
            "MrpExecute" => _serviceProvider.GetRequiredService<MrpExecuteViewModel>(),
            "OrderList" => _serviceProvider.GetRequiredService<OrderListViewModel>(),
            "WorkOrderList" => _serviceProvider.GetRequiredService<WorkOrderListViewModel>(),
            "StockList" => _serviceProvider.GetRequiredService<StockListViewModel>(),
            "ReportList" => _serviceProvider.GetRequiredService<ReportListViewModel>(),
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
            "PurchaseOrderList" => "発注一覧",
            "PurchaseOrderEdit" => "発注登録/編集",
            "MrpExecute" => "MRP 実行",
            "OrderList" => "オーダ照会",
            "WorkOrderList" => "作業指示一覧",
            "StockList" => "在庫照会",
            "ReportList" => "帳票出力",
            _ => viewName
        };
    }
}
