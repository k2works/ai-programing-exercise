using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Web.Models;

namespace ProductionManagement.Web.Controllers;

/// <summary>
/// MRP（所要量展開）Controller
/// </summary>
public class MrpController : Controller
{
    private readonly MrpService _mrpService;
    private readonly IOrderRepository _orderRepository;
    private readonly ILogger<MrpController> _logger;

    public MrpController(
        MrpService mrpService,
        IOrderRepository orderRepository,
        ILogger<MrpController> logger)
    {
        _mrpService = mrpService;
        _orderRepository = orderRepository;
        _logger = logger;
    }

    /// <summary>
    /// MRP 実行画面
    /// </summary>
    public async Task<IActionResult> Execute()
    {
        var orders = await _orderRepository.FindAllAsync();
        var viewModel = new MrpExecuteViewModel
        {
            OrderOptions = orders
                .Where(o => o.Status == Domain.Models.Plan.PlanStatus.Confirmed)
                .Select(o => new SelectListItem
                {
                    Value = o.Id.ToString(),
                    Text = $"{o.OrderNumber} - {o.ItemCode} ({o.DueDate:yyyy/MM/dd})"
                })
                .ToList()
        };
        return View(viewModel);
    }

    /// <summary>
    /// MRP 実行処理
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Execute(MrpExecuteViewModel model)
    {
        if (!ModelState.IsValid || !model.OrderId.HasValue)
        {
            var orders = await _orderRepository.FindAllAsync();
            model.OrderOptions = orders
                .Where(o => o.Status == Domain.Models.Plan.PlanStatus.Confirmed)
                .Select(o => new SelectListItem
                {
                    Value = o.Id.ToString(),
                    Text = $"{o.OrderNumber} - {o.ItemCode} ({o.DueDate:yyyy/MM/dd})"
                })
                .ToList();
            return View(model);
        }

        try
        {
            var requirements = await _mrpService.ExplodeRequirementsAsync(model.OrderId.Value);
            TempData["Success"] = $"MRP を実行しました。{requirements.Count} 件の所要が展開されました。";
            return RedirectToAction(nameof(Result), new { orderId = model.OrderId.Value });
        }
        catch (ArgumentException ex)
        {
            _logger.LogError(ex, "MRP 実行に失敗しました: OrderId={OrderId}", model.OrderId);
            TempData["Error"] = ex.Message;
            return RedirectToAction(nameof(Execute));
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "MRP 実行に失敗しました: OrderId={OrderId}", model.OrderId);
            TempData["Error"] = "MRP の実行に失敗しました。";
            return RedirectToAction(nameof(Execute));
        }
    }

    /// <summary>
    /// MRP 結果表示
    /// </summary>
    public async Task<IActionResult> Result(int orderId)
    {
        var order = await _orderRepository.FindByIdAsync(orderId);
        if (order == null)
        {
            return NotFound();
        }

        var viewModel = new MrpResultViewModel
        {
            Order = OrderViewModel.FromDomain(order),
            Requirements = order.Requirements?
                .Select(RequirementViewModel.FromDomain)
                .ToList() ?? []
        };

        return View(viewModel);
    }
}
