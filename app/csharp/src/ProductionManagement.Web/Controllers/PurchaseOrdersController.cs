using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.Web.Models;

namespace ProductionManagement.Web.Controllers;

/// <summary>
/// 発注管理 Controller
/// </summary>
public class PurchaseOrdersController : Controller
{
    private readonly IPurchaseOrderUseCase _purchaseOrderUseCase;
    private readonly ISupplierUseCase _supplierUseCase;
    private readonly IItemUseCase _itemUseCase;
    private readonly ILogger<PurchaseOrdersController> _logger;

    public PurchaseOrdersController(
        IPurchaseOrderUseCase purchaseOrderUseCase,
        ISupplierUseCase supplierUseCase,
        IItemUseCase itemUseCase,
        ILogger<PurchaseOrdersController> logger)
    {
        _purchaseOrderUseCase = purchaseOrderUseCase;
        _supplierUseCase = supplierUseCase;
        _itemUseCase = itemUseCase;
        _logger = logger;
    }

    /// <summary>
    /// 発注一覧
    /// </summary>
    public async Task<IActionResult> Index(PurchaseOrderSearchViewModel search)
    {
        var allOrders = await _purchaseOrderUseCase.GetAllOrdersAsync();

        // フィルタリング
        var filteredOrders = allOrders.AsEnumerable();

        if (!string.IsNullOrEmpty(search.Status))
        {
            var status = Enum.Parse<PurchaseOrderStatus>(search.Status);
            filteredOrders = filteredOrders.Where(o => o.Status == status);
        }

        if (search.OrderDateFrom.HasValue)
        {
            filteredOrders = filteredOrders.Where(o => o.OrderDate >= search.OrderDateFrom.Value);
        }

        if (search.OrderDateTo.HasValue)
        {
            filteredOrders = filteredOrders.Where(o => o.OrderDate <= search.OrderDateTo.Value);
        }

        if (!string.IsNullOrEmpty(search.Keyword))
        {
            var keyword = search.Keyword.ToLower();
            filteredOrders = filteredOrders.Where(o =>
                o.PurchaseOrderNumber.ToLower().Contains(keyword) ||
                o.SupplierCode.ToLower().Contains(keyword));
        }

        search.StatusOptions = GetStatusSelectList();
        var viewModel = new PurchaseOrderListViewModel
        {
            PurchaseOrders = filteredOrders.Select(PurchaseOrderViewModel.FromDomain).ToList(),
            Search = search
        };

        return View(viewModel);
    }

    /// <summary>
    /// 発注詳細
    /// </summary>
    public async Task<IActionResult> Details(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return NotFound();
        }

        try
        {
            var order = await _purchaseOrderUseCase.GetOrderAsync(id);
            var viewModel = PurchaseOrderViewModel.FromDomain(order);

            // 取引先名を取得
            try
            {
                var supplier = await _supplierUseCase.GetSupplierByCodeAsync(order.SupplierCode);
                viewModel.SupplierName = supplier.SupplierName;
            }
            catch (SupplierNotFoundException)
            {
                // 取引先が見つからない場合はコードのみ表示
            }

            return View(viewModel);
        }
        catch (PurchaseOrderNotFoundException)
        {
            return NotFound();
        }
    }

    /// <summary>
    /// 発注登録画面
    /// </summary>
    public async Task<IActionResult> Create()
    {
        var viewModel = new PurchaseOrderCreateViewModel
        {
            SupplierOptions = await GetSupplierSelectListAsync(),
            ItemOptions = await GetItemSelectListAsync()
        };
        return View(viewModel);
    }

    /// <summary>
    /// 発注登録処理
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Create(PurchaseOrderCreateViewModel model)
    {
        if (!ModelState.IsValid || model.Details.Count == 0)
        {
            if (model.Details.Count == 0)
            {
                ModelState.AddModelError("", "発注明細を1件以上入力してください");
            }
            model.SupplierOptions = await GetSupplierSelectListAsync();
            model.ItemOptions = await GetItemSelectListAsync();
            return View(model);
        }

        try
        {
            var command = new PurchaseOrderCreateCommand
            {
                SupplierCode = model.SupplierCode!,
                OrderDate = model.OrderDate,
                OrdererCode = model.OrdererCode,
                DepartmentCode = model.DepartmentCode,
                TaxRate = model.TaxRate,
                Remarks = model.Remarks,
                Details = model.Details.Select(d => new PurchaseOrderDetailCommand
                {
                    ItemCode = d.ItemCode!,
                    OrderQuantity = d.OrderQuantity,
                    ExpectedReceivingDate = d.ExpectedReceivingDate,
                    DeliveryLocationCode = d.DeliveryLocationCode
                }).ToList()
            };

            var createdOrder = await _purchaseOrderUseCase.CreateOrderAsync(command);
            TempData["Success"] = $"発注「{createdOrder.PurchaseOrderNumber}」を登録しました。";
            return RedirectToAction(nameof(Details), new { id = createdOrder.PurchaseOrderNumber });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "発注登録に失敗しました");
            TempData["Error"] = "発注の登録に失敗しました。";
            model.SupplierOptions = await GetSupplierSelectListAsync();
            model.ItemOptions = await GetItemSelectListAsync();
            return View(model);
        }
    }

    /// <summary>
    /// 発注確定
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Confirm(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return NotFound();
        }

        try
        {
            await _purchaseOrderUseCase.ConfirmOrderAsync(id);
            TempData["Success"] = $"発注「{id}」を確定しました。";
            return RedirectToAction(nameof(Details), new { id });
        }
        catch (PurchaseOrderNotFoundException)
        {
            return NotFound();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "発注確定に失敗しました: {OrderNumber}", id);
            TempData["Error"] = "発注の確定に失敗しました。";
            return RedirectToAction(nameof(Details), new { id });
        }
    }

    /// <summary>
    /// 発注取消
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Cancel(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return NotFound();
        }

        try
        {
            await _purchaseOrderUseCase.CancelOrderAsync(id);
            TempData["Success"] = $"発注「{id}」を取消しました。";
            return RedirectToAction(nameof(Index));
        }
        catch (PurchaseOrderNotFoundException)
        {
            return NotFound();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "発注取消に失敗しました: {OrderNumber}", id);
            TempData["Error"] = "発注の取消に失敗しました。";
            return RedirectToAction(nameof(Details), new { id });
        }
    }

    private static List<SelectListItem> GetStatusSelectList()
    {
        return Enum.GetValues<PurchaseOrderStatus>()
            .Select(s => new SelectListItem
            {
                Value = s.ToString(),
                Text = s.GetDisplayName()
            })
            .ToList();
    }

    private async Task<List<SelectListItem>> GetSupplierSelectListAsync()
    {
        var suppliers = await _supplierUseCase.GetSuppliersByTypeAsync(SupplierType.Vendor);
        return suppliers
            .Select(s => new SelectListItem
            {
                Value = s.SupplierCode,
                Text = $"{s.SupplierCode} - {s.SupplierName}"
            })
            .ToList();
    }

    private async Task<List<SelectListItem>> GetItemSelectListAsync()
    {
        var items = await _itemUseCase.GetAllItemsAsync();
        return items
            .Select(i => new SelectListItem
            {
                Value = i.ItemCode,
                Text = $"{i.ItemCode} - {i.ItemName}"
            })
            .ToList();
    }
}
