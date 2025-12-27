using System.Diagnostics;
using Microsoft.AspNetCore.Mvc;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Web.Models;

namespace ProductionManagement.Web.Controllers;

/// <summary>
/// ホーム Controller
/// ダッシュボードと基本画面を提供
/// </summary>
public class HomeController : Controller
{
    private readonly IItemUseCase _itemUseCase;
    private readonly IPurchaseOrderUseCase _purchaseOrderUseCase;
    private readonly IWorkOrderUseCase _workOrderUseCase;
    private readonly IInventoryUseCase _inventoryUseCase;
    private readonly ILogger<HomeController> _logger;

    public HomeController(
        IItemUseCase itemUseCase,
        IPurchaseOrderUseCase purchaseOrderUseCase,
        IWorkOrderUseCase workOrderUseCase,
        IInventoryUseCase inventoryUseCase,
        ILogger<HomeController> logger)
    {
        _itemUseCase = itemUseCase;
        _purchaseOrderUseCase = purchaseOrderUseCase;
        _workOrderUseCase = workOrderUseCase;
        _inventoryUseCase = inventoryUseCase;
        _logger = logger;
    }

    /// <summary>
    /// ダッシュボード（トップページ）
    /// </summary>
    public async Task<IActionResult> Index()
    {
        try
        {
            // 品目数を取得
            var items = await _itemUseCase.GetAllItemsAsync();
            ViewBag.ItemCount = items.Count;

            // 発注数を取得
            var purchaseOrders = await _purchaseOrderUseCase.GetAllOrdersAsync();
            ViewBag.PurchaseOrderCount = purchaseOrders.Count;

            // 作業指示数を取得
            var workOrders = await _workOrderUseCase.GetAllWorkOrdersAsync();
            ViewBag.WorkOrderCount = workOrders.Count;

            // 在庫サマリー数を取得
            var inventorySummaries = await _inventoryUseCase.GetInventorySummaryAsync();
            ViewBag.InventoryCount = inventorySummaries.Count;
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "ダッシュボード情報の取得に失敗しました");
            ViewBag.ItemCount = 0;
            ViewBag.PurchaseOrderCount = 0;
            ViewBag.WorkOrderCount = 0;
            ViewBag.InventoryCount = 0;
        }

        return View();
    }

    /// <summary>
    /// プライバシーポリシー
    /// </summary>
    public IActionResult Privacy()
    {
        return View();
    }

    /// <summary>
    /// エラー画面
    /// </summary>
    [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
    public IActionResult Error()
    {
        return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
    }
}
