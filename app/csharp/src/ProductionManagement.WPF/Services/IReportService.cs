using ProductionManagement.Domain.Models.Inventory;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.WPF.Services;

/// <summary>
/// 帳票出力サービス インターフェース
/// </summary>
public interface IReportService
{
    // Excel 出力

    /// <summary>
    /// 在庫一覧を Excel 出力
    /// </summary>
    Task ExportStockListToExcelAsync(IEnumerable<Stock> stocks, string filePath);

    /// <summary>
    /// 発注書を Excel 出力
    /// </summary>
    Task ExportPurchaseOrderToExcelAsync(PurchaseOrder order, IEnumerable<PurchaseOrderDetail> details, string filePath);

    /// <summary>
    /// 作業指示書を Excel 出力
    /// </summary>
    Task ExportWorkOrderToExcelAsync(WorkOrder workOrder, string filePath);

    // PDF 出力

    /// <summary>
    /// 在庫一覧を PDF 出力
    /// </summary>
    Task ExportStockListToPdfAsync(IEnumerable<Stock> stocks, string filePath);

    /// <summary>
    /// 発注書を PDF 出力
    /// </summary>
    Task ExportPurchaseOrderToPdfAsync(PurchaseOrder order, IEnumerable<PurchaseOrderDetail> details, string filePath);

    /// <summary>
    /// 作業指示書を PDF 出力
    /// </summary>
    Task ExportWorkOrderToPdfAsync(WorkOrder workOrder, string filePath);
}
