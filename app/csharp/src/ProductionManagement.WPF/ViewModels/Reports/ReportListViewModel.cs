using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using Microsoft.Win32;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.WPF.Services;

namespace ProductionManagement.WPF.ViewModels.Reports;

/// <summary>
/// 帳票出力画面の帳票種別
/// </summary>
public enum ReportType
{
    StockList,
    PurchaseOrder,
    WorkOrder
}

/// <summary>
/// 帳票出力 ViewModel
/// </summary>
public partial class ReportListViewModel : ObservableObject
{
    private readonly IReportService _reportService;
    private readonly IStockRepository _stockRepository;
    private readonly IPurchaseOrderRepository _purchaseOrderRepository;
    private readonly IPurchaseOrderDetailRepository _purchaseOrderDetailRepository;
    private readonly IWorkOrderRepository _workOrderRepository;
    private readonly IDialogService _dialogService;

    public ReportListViewModel(
        IReportService reportService,
        IStockRepository stockRepository,
        IPurchaseOrderRepository purchaseOrderRepository,
        IPurchaseOrderDetailRepository purchaseOrderDetailRepository,
        IWorkOrderRepository workOrderRepository,
        IDialogService dialogService)
    {
        _reportService = reportService;
        _stockRepository = stockRepository;
        _purchaseOrderRepository = purchaseOrderRepository;
        _purchaseOrderDetailRepository = purchaseOrderDetailRepository;
        _workOrderRepository = workOrderRepository;
        _dialogService = dialogService;

        // 帳票種別の選択肢
        ReportTypes =
        [
            new ReportTypeItem(ReportType.StockList, "在庫一覧"),
            new ReportTypeItem(ReportType.PurchaseOrder, "発注書"),
            new ReportTypeItem(ReportType.WorkOrder, "作業指示書")
        ];

        SelectedReportType = ReportTypes[0];
    }

    /// <summary>
    /// 帳票種別一覧
    /// </summary>
    public ObservableCollection<ReportTypeItem> ReportTypes { get; }

    /// <summary>
    /// 選択された帳票種別
    /// </summary>
    [ObservableProperty]
    private ReportTypeItem _selectedReportType;

    /// <summary>
    /// 対象コード（発注番号、作業指示番号など）
    /// </summary>
    [ObservableProperty]
    private string _targetCode = string.Empty;

    [ObservableProperty]
    private bool _isLoading;

    /// <summary>
    /// 対象コード入力欄の表示有無
    /// </summary>
    public bool ShowTargetCodeInput => SelectedReportType?.Type != ReportType.StockList;

    partial void OnSelectedReportTypeChanged(ReportTypeItem value)
    {
        OnPropertyChanged(nameof(ShowTargetCodeInput));
    }

    /// <summary>
    /// Excel 出力コマンド
    /// </summary>
    [RelayCommand]
    private async Task ExportToExcelAsync()
    {
        try
        {
            IsLoading = true;

            // ファイル保存ダイアログ
            var dialog = new SaveFileDialog
            {
                Filter = "Excel ファイル (*.xlsx)|*.xlsx",
                DefaultExt = ".xlsx",
                FileName = GetDefaultFileName(".xlsx")
            };

            if (dialog.ShowDialog() != true)
                return;

            var filePath = dialog.FileName;

            switch (SelectedReportType.Type)
            {
                case ReportType.StockList:
                    await ExportStockListToExcelAsync(filePath);
                    break;

                case ReportType.PurchaseOrder:
                    await ExportPurchaseOrderToExcelAsync(filePath);
                    break;

                case ReportType.WorkOrder:
                    await ExportWorkOrderToExcelAsync(filePath);
                    break;
            }

            await _dialogService.ShowInfoAsync("完了", $"帳票を出力しました。\n{filePath}");
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", $"帳票出力に失敗しました。\n{ex.Message}");
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// PDF 出力コマンド
    /// </summary>
    [RelayCommand]
    private async Task ExportToPdfAsync()
    {
        try
        {
            IsLoading = true;

            // ファイル保存ダイアログ
            var dialog = new SaveFileDialog
            {
                Filter = "PDF ファイル (*.pdf)|*.pdf",
                DefaultExt = ".pdf",
                FileName = GetDefaultFileName(".pdf")
            };

            if (dialog.ShowDialog() != true)
                return;

            var filePath = dialog.FileName;

            switch (SelectedReportType.Type)
            {
                case ReportType.StockList:
                    await ExportStockListToPdfAsync(filePath);
                    break;

                case ReportType.PurchaseOrder:
                    await ExportPurchaseOrderToPdfAsync(filePath);
                    break;

                case ReportType.WorkOrder:
                    await ExportWorkOrderToPdfAsync(filePath);
                    break;
            }

            await _dialogService.ShowInfoAsync("完了", $"帳票を出力しました。\n{filePath}");
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", $"帳票出力に失敗しました。\n{ex.Message}");
        }
        finally
        {
            IsLoading = false;
        }
    }

    private string GetDefaultFileName(string extension)
    {
        var timestamp = DateTime.Now.ToString("yyyyMMdd_HHmmss");
        return SelectedReportType.Type switch
        {
            ReportType.StockList => $"在庫一覧_{timestamp}{extension}",
            ReportType.PurchaseOrder => $"発注書_{TargetCode}_{timestamp}{extension}",
            ReportType.WorkOrder => $"作業指示書_{TargetCode}_{timestamp}{extension}",
            _ => $"帳票_{timestamp}{extension}"
        };
    }

    // Excel 出力メソッド

    private async Task ExportStockListToExcelAsync(string filePath)
    {
        var stocks = await _stockRepository.FindAllAsync();
        await _reportService.ExportStockListToExcelAsync(stocks, filePath);
    }

    private async Task ExportPurchaseOrderToExcelAsync(string filePath)
    {
        if (string.IsNullOrWhiteSpace(TargetCode))
        {
            await _dialogService.ShowWarningAsync("警告", "発注番号を入力してください。");
            return;
        }

        var order = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(TargetCode);
        if (order == null)
        {
            await _dialogService.ShowWarningAsync("警告", $"発注番号 {TargetCode} が見つかりません。");
            return;
        }

        var details = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAsync(TargetCode);
        await _reportService.ExportPurchaseOrderToExcelAsync(order, details, filePath);
    }

    private async Task ExportWorkOrderToExcelAsync(string filePath)
    {
        if (string.IsNullOrWhiteSpace(TargetCode))
        {
            await _dialogService.ShowWarningAsync("警告", "作業指示番号を入力してください。");
            return;
        }

        var workOrder = await _workOrderRepository.FindByWorkOrderNumberAsync(TargetCode);
        if (workOrder == null)
        {
            await _dialogService.ShowWarningAsync("警告", $"作業指示番号 {TargetCode} が見つかりません。");
            return;
        }

        await _reportService.ExportWorkOrderToExcelAsync(workOrder, filePath);
    }

    // PDF 出力メソッド

    private async Task ExportStockListToPdfAsync(string filePath)
    {
        var stocks = await _stockRepository.FindAllAsync();
        await _reportService.ExportStockListToPdfAsync(stocks, filePath);
    }

    private async Task ExportPurchaseOrderToPdfAsync(string filePath)
    {
        if (string.IsNullOrWhiteSpace(TargetCode))
        {
            await _dialogService.ShowWarningAsync("警告", "発注番号を入力してください。");
            return;
        }

        var order = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(TargetCode);
        if (order == null)
        {
            await _dialogService.ShowWarningAsync("警告", $"発注番号 {TargetCode} が見つかりません。");
            return;
        }

        var details = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAsync(TargetCode);
        await _reportService.ExportPurchaseOrderToPdfAsync(order, details, filePath);
    }

    private async Task ExportWorkOrderToPdfAsync(string filePath)
    {
        if (string.IsNullOrWhiteSpace(TargetCode))
        {
            await _dialogService.ShowWarningAsync("警告", "作業指示番号を入力してください。");
            return;
        }

        var workOrder = await _workOrderRepository.FindByWorkOrderNumberAsync(TargetCode);
        if (workOrder == null)
        {
            await _dialogService.ShowWarningAsync("警告", $"作業指示番号 {TargetCode} が見つかりません。");
            return;
        }

        await _reportService.ExportWorkOrderToPdfAsync(workOrder, filePath);
    }
}

/// <summary>
/// 帳票種別アイテム
/// </summary>
public record ReportTypeItem(ReportType Type, string DisplayName)
{
    public override string ToString() => DisplayName;
}
