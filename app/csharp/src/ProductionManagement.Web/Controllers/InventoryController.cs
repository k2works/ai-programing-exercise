using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Web.Models;

namespace ProductionManagement.Web.Controllers;

/// <summary>
/// 在庫管理 Controller
/// </summary>
public class InventoryController : Controller
{
    private readonly IInventoryUseCase _inventoryUseCase;
    private readonly IItemUseCase _itemUseCase;
    private readonly ILogger<InventoryController> _logger;

    public InventoryController(
        IInventoryUseCase inventoryUseCase,
        IItemUseCase itemUseCase,
        ILogger<InventoryController> logger)
    {
        _inventoryUseCase = inventoryUseCase;
        _itemUseCase = itemUseCase;
        _logger = logger;
    }

    /// <summary>
    /// 在庫一覧
    /// </summary>
    public async Task<IActionResult> Index(InventorySearchViewModel search)
    {
        // 在庫サマリーを取得
        var summaries = await _inventoryUseCase.GetInventorySummaryAsync();
        var summaryViewModels = summaries.Select(InventorySummaryViewModel.FromDomain).ToList();

        // フィルタリング
        if (!string.IsNullOrEmpty(search.ItemCode))
        {
            var keyword = search.ItemCode.ToLower();
            summaryViewModels = summaryViewModels
                .Where(s => s.ItemCode?.ToLower().Contains(keyword) == true ||
                           s.ItemName?.ToLower().Contains(keyword) == true)
                .ToList();
        }

        if (!string.IsNullOrEmpty(search.StockState))
        {
            var state = Enum.Parse<StockState>(search.StockState);
            summaryViewModels = summaryViewModels.Where(s => s.StockState == state).ToList();
        }

        search.StockStateOptions = GetStockStateSelectList();

        var viewModel = new InventoryListViewModel
        {
            Summaries = summaryViewModels,
            Search = search
        };

        return View(viewModel);
    }

    /// <summary>
    /// 在庫詳細（品目別ロケーション在庫）
    /// </summary>
    public async Task<IActionResult> Details(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return NotFound();
        }

        try
        {
            // 品目情報を取得
            var item = await _itemUseCase.GetItemByCodeAsync(id);

            // 品目の在庫を取得
            var query = new InventoryQuery(ItemCode: id);
            var stocks = await _inventoryUseCase.GetInventoryAsync(query);
            var stockViewModels = stocks.Select(s =>
            {
                var vm = StockViewModel.FromDomain(s);
                vm.ItemName = item.ItemName;
                return vm;
            }).ToList();

            // サマリーを取得
            var summaries = await _inventoryUseCase.GetInventorySummaryAsync();
            var summary = summaries.FirstOrDefault(s => s.ItemCode == id);

            ViewBag.ItemCode = id;
            ViewBag.ItemName = item.ItemName;
            ViewBag.Summary = summary != null ? InventorySummaryViewModel.FromDomain(summary) : null;

            return View(stockViewModels);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "在庫詳細の取得に失敗しました: ItemCode={ItemCode}", id);
            return NotFound();
        }
    }

    /// <summary>
    /// 在庫不足品目一覧
    /// </summary>
    public async Task<IActionResult> Shortage()
    {
        var shortageItems = await _inventoryUseCase.GetShortageItemsAsync();
        var viewModels = shortageItems.Select(InventorySummaryViewModel.FromDomain).ToList();

        return View(viewModels);
    }

    private static List<SelectListItem> GetStockStateSelectList()
    {
        return new List<SelectListItem>
        {
            new() { Value = StockState.Normal.ToString(), Text = "正常" },
            new() { Value = StockState.Shortage.ToString(), Text = "不足" },
            new() { Value = StockState.Excess.ToString(), Text = "過剰" }
        };
    }
}
