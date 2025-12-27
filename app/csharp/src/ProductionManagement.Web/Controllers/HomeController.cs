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
    private readonly ILogger<HomeController> _logger;

    public HomeController(IItemUseCase itemUseCase, ILogger<HomeController> logger)
    {
        _itemUseCase = itemUseCase;
        _logger = logger;
    }

    /// <summary>
    /// ダッシュボード（トップページ）
    /// </summary>
    public async Task<IActionResult> Index()
    {
        try
        {
            // 品目数を取得してダッシュボードに表示
            var items = await _itemUseCase.GetAllItemsAsync();
            ViewBag.ItemCount = items.Count;
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "ダッシュボード情報の取得に失敗しました");
            ViewBag.ItemCount = 0;
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
