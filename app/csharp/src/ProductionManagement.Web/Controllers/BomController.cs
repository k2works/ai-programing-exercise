using Microsoft.AspNetCore.Mvc;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Web.Models;

namespace ProductionManagement.Web.Controllers;

/// <summary>
/// BOM（部品構成表） Controller
/// </summary>
public class BomController : Controller
{
    private readonly BomService _bomService;
    private readonly ILogger<BomController> _logger;

    public BomController(BomService bomService, ILogger<BomController> logger)
    {
        _bomService = bomService;
        _logger = logger;
    }

    /// <summary>
    /// 部品展開画面
    /// </summary>
    public async Task<IActionResult> Explode(string? itemCode)
    {
        var viewModel = new BomExplodeViewModel { ItemCode = itemCode };

        if (!string.IsNullOrEmpty(itemCode))
        {
            try
            {
                var bomTree = await _bomService.ExplodeBomAsync(itemCode);
                viewModel.BomTree = BomNodeViewModel.FromDomain(bomTree);
            }
            catch (ItemNotFoundException)
            {
                TempData["Error"] = $"品目「{itemCode}」が見つかりません。";
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "BOM 展開に失敗しました");
                TempData["Error"] = "BOM 展開に失敗しました。";
            }
        }

        return View(viewModel);
    }

    /// <summary>
    /// 使用先照会画面
    /// </summary>
    public async Task<IActionResult> WhereUsed(string? itemCode)
    {
        var viewModel = new WhereUsedViewModel { ItemCode = itemCode };

        if (!string.IsNullOrEmpty(itemCode))
        {
            try
            {
                var results = await _bomService.WhereUsedAsync(itemCode);
                viewModel.Results = results.Select(WhereUsedResultViewModel.FromDomain).ToList();
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "使用先照会に失敗しました");
                TempData["Error"] = "使用先照会に失敗しました。";
            }
        }

        return View(viewModel);
    }
}
