using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Web.Models;

namespace ProductionManagement.Web.Controllers;

/// <summary>
/// 品目マスタ Controller
/// </summary>
public class ItemsController : Controller
{
    private readonly IItemUseCase _itemUseCase;
    private readonly ILogger<ItemsController> _logger;

    public ItemsController(IItemUseCase itemUseCase, ILogger<ItemsController> logger)
    {
        _itemUseCase = itemUseCase;
        _logger = logger;
    }

    /// <summary>
    /// 品目一覧
    /// </summary>
    public async Task<IActionResult> Index(ItemSearchViewModel search)
    {
        var allItems = await _itemUseCase.GetAllItemsAsync();

        // フィルタリング
        var filteredItems = allItems.AsEnumerable();

        if (!string.IsNullOrEmpty(search.Category))
        {
            var category = Enum.Parse<ItemCategory>(search.Category);
            filteredItems = filteredItems.Where(i => i.ItemCategory == category);
        }

        if (!string.IsNullOrEmpty(search.Keyword))
        {
            var keyword = search.Keyword.ToLower();
            filteredItems = filteredItems.Where(i =>
                i.ItemCode.ToLower().Contains(keyword) ||
                i.ItemName.ToLower().Contains(keyword));
        }

        search.CategoryOptions = GetCategorySelectList();
        var viewModel = new ItemListViewModel
        {
            Items = filteredItems.Select(ItemViewModel.FromDomain).ToList(),
            Search = search
        };

        return View(viewModel);
    }

    /// <summary>
    /// 品目詳細
    /// </summary>
    public async Task<IActionResult> Details(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return NotFound();
        }

        try
        {
            var item = await _itemUseCase.GetItemByCodeAsync(id);
            return View(ItemViewModel.FromDomain(item));
        }
        catch (ItemNotFoundException)
        {
            return NotFound();
        }
    }

    /// <summary>
    /// 品目登録画面
    /// </summary>
    public IActionResult Create()
    {
        ViewBag.Categories = GetCategorySelectList();
        return View(new ItemViewModel());
    }

    /// <summary>
    /// 品目登録処理
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Create(ItemViewModel model)
    {
        if (!ModelState.IsValid)
        {
            ViewBag.Categories = GetCategorySelectList();
            return View(model);
        }

        try
        {
            var command = new CreateItemCommand(
                ItemCode: model.ItemCode!,
                ItemName: model.ItemName!,
                Category: model.ItemCategory,
                UnitCode: model.UnitCode,
                LeadTime: model.LeadTime,
                SafetyLeadTime: model.SafetyLeadTime,
                SafetyStock: model.SafetyStock,
                YieldRate: model.YieldRate,
                MinLotSize: model.MinLotSize,
                LotIncrement: model.LotIncrement,
                MaxLotSize: model.MaxLotSize,
                ShelfLife: model.ShelfLife
            );

            await _itemUseCase.CreateItemAsync(command);
            TempData["Success"] = $"品目「{model.ItemCode}」を登録しました。";
            return RedirectToAction(nameof(Index));
        }
        catch (DuplicateItemException ex)
        {
            ModelState.AddModelError("ItemCode", ex.Message);
            ViewBag.Categories = GetCategorySelectList();
            return View(model);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "品目登録に失敗しました");
            TempData["Error"] = "品目の登録に失敗しました。";
            ViewBag.Categories = GetCategorySelectList();
            return View(model);
        }
    }

    /// <summary>
    /// 品目編集画面
    /// </summary>
    public async Task<IActionResult> Edit(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return NotFound();
        }

        try
        {
            var item = await _itemUseCase.GetItemByCodeAsync(id);
            ViewBag.Categories = GetCategorySelectList();
            return View(ItemViewModel.FromDomain(item));
        }
        catch (ItemNotFoundException)
        {
            return NotFound();
        }
    }

    /// <summary>
    /// 品目編集処理
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Edit(string id, ItemViewModel model)
    {
        if (id != model.ItemCode)
        {
            return NotFound();
        }

        if (!ModelState.IsValid)
        {
            ViewBag.Categories = GetCategorySelectList();
            return View(model);
        }

        try
        {
            var command = new UpdateItemCommand(
                ItemCode: model.ItemCode!,
                ItemName: model.ItemName,
                Category: model.ItemCategory,
                UnitCode: model.UnitCode,
                LeadTime: model.LeadTime,
                SafetyLeadTime: model.SafetyLeadTime,
                SafetyStock: model.SafetyStock,
                YieldRate: model.YieldRate,
                MinLotSize: model.MinLotSize,
                LotIncrement: model.LotIncrement,
                MaxLotSize: model.MaxLotSize,
                ShelfLife: model.ShelfLife
            );

            await _itemUseCase.UpdateItemAsync(command);
            TempData["Success"] = $"品目「{model.ItemCode}」を更新しました。";
            return RedirectToAction(nameof(Index));
        }
        catch (ItemNotFoundException)
        {
            return NotFound();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "品目更新に失敗しました");
            TempData["Error"] = "品目の更新に失敗しました。";
            ViewBag.Categories = GetCategorySelectList();
            return View(model);
        }
    }

    /// <summary>
    /// 品目削除確認画面
    /// </summary>
    public async Task<IActionResult> Delete(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return NotFound();
        }

        try
        {
            var item = await _itemUseCase.GetItemByCodeAsync(id);
            return View(ItemViewModel.FromDomain(item));
        }
        catch (ItemNotFoundException)
        {
            return NotFound();
        }
    }

    /// <summary>
    /// 品目削除処理
    /// </summary>
    [HttpPost, ActionName("Delete")]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> DeleteConfirmed(string id)
    {
        try
        {
            await _itemUseCase.DeleteItemAsync(id);
            TempData["Success"] = $"品目「{id}」を削除しました。";
            return RedirectToAction(nameof(Index));
        }
        catch (ItemNotFoundException)
        {
            return NotFound();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "品目削除に失敗しました");
            TempData["Error"] = "品目の削除に失敗しました。";
            return RedirectToAction(nameof(Index));
        }
    }

    private static List<SelectListItem> GetCategorySelectList()
    {
        return Enum.GetValues<ItemCategory>()
            .Select(c => new SelectListItem
            {
                Value = c.ToString(),
                Text = c.GetDisplayName()
            })
            .ToList();
    }
}
