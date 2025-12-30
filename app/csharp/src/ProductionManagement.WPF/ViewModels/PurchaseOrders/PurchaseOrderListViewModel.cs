using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.WPF.Services;
using ProductionManagement.WPF.ViewModels.Items;

namespace ProductionManagement.WPF.ViewModels.PurchaseOrders;

/// <summary>
/// 発注一覧 ViewModel
/// </summary>
public partial class PurchaseOrderListViewModel : ObservableObject
{
    private readonly IPurchaseOrderUseCase _purchaseOrderUseCase;
    private readonly INavigationService _navigationService;
    private readonly IDialogService _dialogService;

    public PurchaseOrderListViewModel(
        IPurchaseOrderUseCase purchaseOrderUseCase,
        INavigationService navigationService,
        IDialogService dialogService)
    {
        _purchaseOrderUseCase = purchaseOrderUseCase;
        _navigationService = navigationService;
        _dialogService = dialogService;

        // 初期値
        DateFrom = DateTime.Today.AddMonths(-1);
        DateTo = DateTime.Today.AddMonths(1);

        _ = LoadAsync();
    }

    [ObservableProperty]
    private ObservableCollection<PurchaseOrder> _orders = [];

    [ObservableProperty]
    [NotifyCanExecuteChangedFor(nameof(EditCommand))]
    [NotifyCanExecuteChangedFor(nameof(ConfirmCommand))]
    [NotifyCanExecuteChangedFor(nameof(CancelCommand))]
    private PurchaseOrder? _selectedOrder;

    [ObservableProperty]
    private DateTime _dateFrom;

    [ObservableProperty]
    private DateTime _dateTo;

    [ObservableProperty]
    private PurchaseOrderStatus? _selectedStatus;

    [ObservableProperty]
    private string _supplierCode = string.Empty;

    [ObservableProperty]
    private bool _isLoading;

    /// <summary>
    /// ステータスリスト（フィルタ用）
    /// </summary>
    public IReadOnlyList<PurchaseOrderStatus?> Statuses { get; } =
    [
        null,
        PurchaseOrderStatus.Creating,
        PurchaseOrderStatus.Ordered,
        PurchaseOrderStatus.PartiallyReceived,
        PurchaseOrderStatus.Received,
        PurchaseOrderStatus.Accepted,
        PurchaseOrderStatus.Cancelled
    ];

    /// <summary>
    /// データ読み込み
    /// </summary>
    [RelayCommand]
    private async Task LoadAsync()
    {
        try
        {
            IsLoading = true;
            var allOrders = await _purchaseOrderUseCase.GetAllOrdersAsync();

            // フィルタ適用
            var filteredOrders = allOrders.AsEnumerable();

            // 日付範囲フィルタ
            var dateFrom = DateOnly.FromDateTime(DateFrom);
            var dateTo = DateOnly.FromDateTime(DateTo);
            filteredOrders = filteredOrders.Where(o => o.OrderDate >= dateFrom && o.OrderDate <= dateTo);

            // ステータスフィルタ
            if (SelectedStatus.HasValue)
            {
                filteredOrders = filteredOrders.Where(o => o.Status == SelectedStatus.Value);
            }

            // 取引先コードフィルタ
            if (!string.IsNullOrWhiteSpace(SupplierCode))
            {
                filteredOrders = filteredOrders.Where(o =>
                    o.SupplierCode.Contains(SupplierCode, StringComparison.OrdinalIgnoreCase));
            }

            Orders = new ObservableCollection<PurchaseOrder>(
                filteredOrders.OrderByDescending(o => o.OrderDate).ThenByDescending(o => o.PurchaseOrderNumber));
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// 新規登録画面へ遷移
    /// </summary>
    [RelayCommand]
    private void Create()
    {
        _navigationService.NavigateTo("PurchaseOrderEdit", new PurchaseOrderEditParameter(Mode: EditMode.Create));
    }

    /// <summary>
    /// 編集画面へ遷移
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanEdit))]
    private void Edit()
    {
        if (SelectedOrder != null)
        {
            _navigationService.NavigateTo("PurchaseOrderEdit", new PurchaseOrderEditParameter(
                Mode: EditMode.Edit,
                OrderNumber: SelectedOrder.PurchaseOrderNumber));
        }
    }

    private bool CanEdit() => SelectedOrder != null;

    /// <summary>
    /// 発注確定
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanConfirm))]
    private async Task ConfirmAsync()
    {
        if (SelectedOrder == null) return;

        var confirmed = await _dialogService.ShowConfirmAsync(
            "確定確認",
            $"発注「{SelectedOrder.PurchaseOrderNumber}」を確定しますか？");

        if (!confirmed) return;

        try
        {
            await _purchaseOrderUseCase.ConfirmOrderAsync(SelectedOrder.PurchaseOrderNumber);
            await _dialogService.ShowInfoAsync("完了", "発注を確定しました");
            await LoadAsync();
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
        }
    }

    private bool CanConfirm() => SelectedOrder?.Status == PurchaseOrderStatus.Creating;

    /// <summary>
    /// 発注取消
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanCancel))]
    private async Task CancelAsync()
    {
        if (SelectedOrder == null) return;

        var confirmed = await _dialogService.ShowConfirmAsync(
            "取消確認",
            $"発注「{SelectedOrder.PurchaseOrderNumber}」を取消しますか？");

        if (!confirmed) return;

        try
        {
            await _purchaseOrderUseCase.CancelOrderAsync(SelectedOrder.PurchaseOrderNumber);
            await _dialogService.ShowInfoAsync("完了", "発注を取消しました");
            await LoadAsync();
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
        }
    }

    private bool CanCancel() =>
        SelectedOrder != null &&
        (SelectedOrder.Status == PurchaseOrderStatus.Creating || SelectedOrder.Status == PurchaseOrderStatus.Ordered);

    /// <summary>
    /// フィルタークリア
    /// </summary>
    [RelayCommand]
    private async Task ClearFilterAsync()
    {
        DateFrom = DateTime.Today.AddMonths(-1);
        DateTo = DateTime.Today.AddMonths(1);
        SelectedStatus = null;
        SupplierCode = string.Empty;
        await LoadAsync();
    }

    /// <summary>
    /// 行ダブルクリック
    /// </summary>
    [RelayCommand]
    private void RowDoubleClick(PurchaseOrder? order)
    {
        if (order != null)
        {
            _navigationService.NavigateTo("PurchaseOrderEdit", new PurchaseOrderEditParameter(
                Mode: EditMode.Edit,
                OrderNumber: order.PurchaseOrderNumber));
        }
    }
}

/// <summary>
/// 発注編集パラメータ
/// </summary>
public record PurchaseOrderEditParameter(EditMode Mode, string? OrderNumber = null);
