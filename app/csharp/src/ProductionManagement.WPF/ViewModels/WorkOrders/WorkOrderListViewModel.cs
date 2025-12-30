using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Process;
using ProductionManagement.WPF.Services;

namespace ProductionManagement.WPF.ViewModels.WorkOrders;

/// <summary>
/// 作業指示一覧 ViewModel
/// </summary>
public partial class WorkOrderListViewModel : ObservableObject
{
    private readonly IWorkOrderRepository _workOrderRepository;
    private readonly IDialogService _dialogService;

    public WorkOrderListViewModel(IWorkOrderRepository workOrderRepository, IDialogService dialogService)
    {
        _workOrderRepository = workOrderRepository;
        _dialogService = dialogService;

        // 初期値
        DateFrom = DateOnly.FromDateTime(DateTime.Today.AddMonths(-1));
        DateTo = DateOnly.FromDateTime(DateTime.Today.AddMonths(1));
    }

    /// <summary>
    /// 作業指示一覧
    /// </summary>
    public ObservableCollection<WorkOrder> WorkOrders { get; } = [];

    /// <summary>
    /// ステータス選択肢
    /// </summary>
    public IReadOnlyList<WorkOrderStatus?> Statuses { get; } =
        [null, WorkOrderStatus.NotStarted, WorkOrderStatus.InProgress, WorkOrderStatus.Completed, WorkOrderStatus.Suspended];

    [ObservableProperty]
    private DateOnly? _dateFrom;

    [ObservableProperty]
    private DateOnly? _dateTo;

    [ObservableProperty]
    private WorkOrderStatus? _selectedStatus;

    [ObservableProperty]
    private string _itemCode = string.Empty;

    [ObservableProperty]
    private WorkOrder? _selectedWorkOrder;

    [ObservableProperty]
    private bool _isLoading;

    /// <summary>
    /// データ読み込み
    /// </summary>
    [RelayCommand]
    private async Task LoadAsync()
    {
        try
        {
            IsLoading = true;
            var workOrders = await _workOrderRepository.FindAllAsync();

            // フィルタリング
            var filtered = workOrders.AsEnumerable();

            if (DateFrom.HasValue)
            {
                filtered = filtered.Where(w => w.PlannedStartDate >= DateFrom.Value);
            }

            if (DateTo.HasValue)
            {
                filtered = filtered.Where(w => w.PlannedEndDate <= DateTo.Value);
            }

            if (SelectedStatus.HasValue)
            {
                filtered = filtered.Where(w => w.Status == SelectedStatus.Value);
            }

            if (!string.IsNullOrWhiteSpace(ItemCode))
            {
                filtered = filtered.Where(w => w.ItemCode.Contains(ItemCode, StringComparison.OrdinalIgnoreCase));
            }

            WorkOrders.Clear();
            foreach (var workOrder in filtered.OrderByDescending(w => w.WorkOrderDate).ThenBy(w => w.WorkOrderNumber))
            {
                WorkOrders.Add(workOrder);
            }
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", $"データの読み込みに失敗しました: {ex.Message}");
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// フィルタクリア
    /// </summary>
    [RelayCommand]
    private void ClearFilter()
    {
        DateFrom = DateOnly.FromDateTime(DateTime.Today.AddMonths(-1));
        DateTo = DateOnly.FromDateTime(DateTime.Today.AddMonths(1));
        SelectedStatus = null;
        ItemCode = string.Empty;
    }

    /// <summary>
    /// 作業開始
    /// </summary>
    [RelayCommand]
    private async Task StartWorkAsync()
    {
        if (SelectedWorkOrder == null)
        {
            return;
        }

        if (SelectedWorkOrder.Status != WorkOrderStatus.NotStarted)
        {
            await _dialogService.ShowInfoAsync("確認", "未着手の作業指示のみ開始できます。");
            return;
        }

        var confirmed = await _dialogService.ShowConfirmAsync(
            "確認",
            $"作業指示 {SelectedWorkOrder.WorkOrderNumber} を開始しますか？");

        if (!confirmed)
        {
            return;
        }

        try
        {
            IsLoading = true;
            await _workOrderRepository.StartWorkAsync(SelectedWorkOrder.WorkOrderNumber, DateOnly.FromDateTime(DateTime.Today));
            await _dialogService.ShowInfoAsync("完了", "作業を開始しました。");
            await LoadAsync();
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
    /// 作業完了
    /// </summary>
    [RelayCommand]
    private async Task CompleteWorkAsync()
    {
        if (SelectedWorkOrder == null)
        {
            return;
        }

        if (SelectedWorkOrder.Status != WorkOrderStatus.InProgress)
        {
            await _dialogService.ShowInfoAsync("確認", "作業中の作業指示のみ完了できます。");
            return;
        }

        var confirmed = await _dialogService.ShowConfirmAsync(
            "確認",
            $"作業指示 {SelectedWorkOrder.WorkOrderNumber} を完了しますか？");

        if (!confirmed)
        {
            return;
        }

        try
        {
            IsLoading = true;
            await _workOrderRepository.CompleteWorkAsync(SelectedWorkOrder.WorkOrderNumber, DateOnly.FromDateTime(DateTime.Today));
            await _dialogService.ShowInfoAsync("完了", "作業を完了しました。");
            await LoadAsync();
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
}
