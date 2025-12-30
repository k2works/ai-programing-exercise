using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.WPF.Services;

namespace ProductionManagement.WPF.ViewModels.Planning;

/// <summary>
/// オーダ照会 ViewModel
/// </summary>
public partial class OrderListViewModel : ObservableObject
{
    private readonly IOrderRepository _orderRepository;
    private readonly IDialogService _dialogService;

    public OrderListViewModel(
        IOrderRepository orderRepository,
        IDialogService dialogService)
    {
        _orderRepository = orderRepository;
        _dialogService = dialogService;

        // 初期値
        DateFrom = DateTime.Today.AddMonths(-1);
        DateTo = DateTime.Today.AddMonths(3);

        _ = LoadAsync();
    }

    [ObservableProperty]
    private ObservableCollection<Order> _orders = [];

    [ObservableProperty]
    private Order? _selectedOrder;

    [ObservableProperty]
    private DateTime _dateFrom;

    [ObservableProperty]
    private DateTime _dateTo;

    [ObservableProperty]
    private OrderType? _selectedOrderType;

    [ObservableProperty]
    private PlanStatus? _selectedStatus;

    [ObservableProperty]
    private string _itemCode = string.Empty;

    [ObservableProperty]
    private bool _isLoading;

    /// <summary>
    /// オーダ種別リスト（フィルタ用）
    /// </summary>
    public IReadOnlyList<OrderType?> OrderTypes { get; } =
    [
        null,
        OrderType.Purchase,
        OrderType.Manufacturing
    ];

    /// <summary>
    /// ステータスリスト（フィルタ用）
    /// </summary>
    public IReadOnlyList<PlanStatus?> Statuses { get; } =
    [
        null,
        PlanStatus.Draft,
        PlanStatus.Confirmed,
        PlanStatus.Expanded,
        PlanStatus.Cancelled
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
            var allOrders = await _orderRepository.FindAllAsync();

            // フィルタ適用
            var filteredOrders = allOrders.AsEnumerable();

            // 日付範囲フィルタ（納期）
            var dateFrom = DateOnly.FromDateTime(DateFrom);
            var dateTo = DateOnly.FromDateTime(DateTo);
            filteredOrders = filteredOrders.Where(o => o.DueDate >= dateFrom && o.DueDate <= dateTo);

            // オーダ種別フィルタ
            if (SelectedOrderType.HasValue)
            {
                filteredOrders = filteredOrders.Where(o => o.OrderType == SelectedOrderType.Value);
            }

            // ステータスフィルタ
            if (SelectedStatus.HasValue)
            {
                filteredOrders = filteredOrders.Where(o => o.Status == SelectedStatus.Value);
            }

            // 品目コードフィルタ
            if (!string.IsNullOrWhiteSpace(ItemCode))
            {
                filteredOrders = filteredOrders.Where(o =>
                    o.ItemCode.Contains(ItemCode, StringComparison.OrdinalIgnoreCase));
            }

            Orders = new ObservableCollection<Order>(
                filteredOrders.OrderBy(o => o.DueDate).ThenBy(o => o.OrderNumber));
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// フィルタークリア
    /// </summary>
    [RelayCommand]
    private async Task ClearFilterAsync()
    {
        DateFrom = DateTime.Today.AddMonths(-1);
        DateTo = DateTime.Today.AddMonths(3);
        SelectedOrderType = null;
        SelectedStatus = null;
        ItemCode = string.Empty;
        await LoadAsync();
    }

    /// <summary>
    /// オーダ確定
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanConfirm))]
    private async Task ConfirmAsync()
    {
        if (SelectedOrder == null) return;

        var confirmed = await _dialogService.ShowConfirmAsync(
            "確定確認",
            $"オーダ「{SelectedOrder.OrderNumber}」を確定しますか？");

        if (!confirmed) return;

        try
        {
            await _orderRepository.UpdateStatusAsync(SelectedOrder.Id, PlanStatus.Confirmed);
            await _dialogService.ShowInfoAsync("完了", "オーダを確定しました");
            await LoadAsync();
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
        }
    }

    private bool CanConfirm() => SelectedOrder?.Status == PlanStatus.Draft;

    /// <summary>
    /// オーダ取消
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanCancel))]
    private async Task CancelAsync()
    {
        if (SelectedOrder == null) return;

        var confirmed = await _dialogService.ShowConfirmAsync(
            "取消確認",
            $"オーダ「{SelectedOrder.OrderNumber}」を取消しますか？");

        if (!confirmed) return;

        try
        {
            await _orderRepository.UpdateStatusAsync(SelectedOrder.Id, PlanStatus.Cancelled);
            await _dialogService.ShowInfoAsync("完了", "オーダを取消しました");
            await LoadAsync();
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
        }
    }

    private bool CanCancel() =>
        SelectedOrder != null &&
        (SelectedOrder.Status == PlanStatus.Draft || SelectedOrder.Status == PlanStatus.Confirmed);
}
