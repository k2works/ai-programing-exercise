using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.Web.Models;

namespace ProductionManagement.Web.Controllers;

/// <summary>
/// 取引先マスタ Controller
/// </summary>
public class SuppliersController : Controller
{
    private readonly ISupplierUseCase _supplierUseCase;
    private readonly ILogger<SuppliersController> _logger;

    public SuppliersController(ISupplierUseCase supplierUseCase, ILogger<SuppliersController> logger)
    {
        _supplierUseCase = supplierUseCase;
        _logger = logger;
    }

    /// <summary>
    /// 取引先一覧
    /// </summary>
    public async Task<IActionResult> Index(SupplierSearchViewModel search)
    {
        var allSuppliers = await _supplierUseCase.GetAllSuppliersAsync();

        // フィルタリング
        var filteredSuppliers = allSuppliers.AsEnumerable();

        if (!string.IsNullOrEmpty(search.SupplierType))
        {
            var supplierType = Enum.Parse<SupplierType>(search.SupplierType);
            filteredSuppliers = filteredSuppliers.Where(s => s.SupplierType == supplierType);
        }

        if (!string.IsNullOrEmpty(search.Keyword))
        {
            var keyword = search.Keyword.ToLower();
            filteredSuppliers = filteredSuppliers.Where(s =>
                s.SupplierCode.ToLower().Contains(keyword) ||
                s.SupplierName.ToLower().Contains(keyword) ||
                (s.SupplierNameKana?.ToLower().Contains(keyword) ?? false));
        }

        search.SupplierTypeOptions = GetSupplierTypeSelectList();
        var viewModel = new SupplierListViewModel
        {
            Suppliers = filteredSuppliers.Select(SupplierViewModel.FromDomain).ToList(),
            Search = search
        };

        return View(viewModel);
    }

    /// <summary>
    /// 取引先詳細
    /// </summary>
    public async Task<IActionResult> Details(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return NotFound();
        }

        try
        {
            var supplier = await _supplierUseCase.GetSupplierByCodeAsync(id);
            return View(SupplierViewModel.FromDomain(supplier));
        }
        catch (SupplierNotFoundException)
        {
            return NotFound();
        }
    }

    /// <summary>
    /// 取引先登録画面
    /// </summary>
    public IActionResult Create()
    {
        ViewBag.SupplierTypes = GetSupplierTypeSelectList();
        return View(new SupplierViewModel());
    }

    /// <summary>
    /// 取引先登録処理
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Create(SupplierViewModel model)
    {
        if (!ModelState.IsValid)
        {
            ViewBag.SupplierTypes = GetSupplierTypeSelectList();
            return View(model);
        }

        try
        {
            var command = new CreateSupplierCommand(
                SupplierCode: model.SupplierCode!,
                SupplierName: model.SupplierName!,
                SupplierType: model.SupplierType,
                SupplierNameKana: model.SupplierNameKana,
                PostalCode: model.PostalCode,
                Address: model.Address,
                PhoneNumber: model.PhoneNumber,
                FaxNumber: model.FaxNumber,
                ContactPerson: model.ContactPerson
            );

            await _supplierUseCase.CreateSupplierAsync(command);
            TempData["Success"] = $"取引先「{model.SupplierCode}」を登録しました。";
            return RedirectToAction(nameof(Index));
        }
        catch (DuplicateSupplierException ex)
        {
            ModelState.AddModelError("SupplierCode", ex.Message);
            ViewBag.SupplierTypes = GetSupplierTypeSelectList();
            return View(model);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "取引先登録に失敗しました");
            TempData["Error"] = "取引先の登録に失敗しました。";
            ViewBag.SupplierTypes = GetSupplierTypeSelectList();
            return View(model);
        }
    }

    private static List<SelectListItem> GetSupplierTypeSelectList()
    {
        return Enum.GetValues<SupplierType>()
            .Select(t => new SelectListItem
            {
                Value = t.ToString(),
                Text = t.GetDisplayName()
            })
            .ToList();
    }
}
