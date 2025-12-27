using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Process;
using ProductionManagement.Web.Models;

namespace ProductionManagement.Web.Controllers;

/// <summary>
/// 作業指示 Controller
/// </summary>
public class WorkOrdersController : Controller
{
    private readonly IWorkOrderUseCase _workOrderUseCase;
    private readonly IOrderRepository _orderRepository;
    private readonly IItemUseCase _itemUseCase;
    private readonly ILogger<WorkOrdersController> _logger;

    public WorkOrdersController(
        IWorkOrderUseCase workOrderUseCase,
        IOrderRepository orderRepository,
        IItemUseCase itemUseCase,
        ILogger<WorkOrdersController> logger)
    {
        _workOrderUseCase = workOrderUseCase;
        _orderRepository = orderRepository;
        _itemUseCase = itemUseCase;
        _logger = logger;
    }

    /// <summary>
    /// 作業指示一覧
    /// </summary>
    public async Task<IActionResult> Index(WorkOrderSearchViewModel search)
    {
        var allWorkOrders = await _workOrderUseCase.GetAllWorkOrdersAsync();

        // フィルタリング
        var filteredWorkOrders = allWorkOrders.AsEnumerable();

        if (!string.IsNullOrEmpty(search.Status))
        {
            var status = Enum.Parse<WorkOrderStatus>(search.Status);
            filteredWorkOrders = filteredWorkOrders.Where(w => w.Status == status);
        }

        if (search.WorkOrderDateFrom.HasValue)
        {
            filteredWorkOrders = filteredWorkOrders.Where(w => w.WorkOrderDate >= search.WorkOrderDateFrom.Value);
        }

        if (search.WorkOrderDateTo.HasValue)
        {
            filteredWorkOrders = filteredWorkOrders.Where(w => w.WorkOrderDate <= search.WorkOrderDateTo.Value);
        }

        if (!string.IsNullOrEmpty(search.Keyword))
        {
            var keyword = search.Keyword.ToLower();
            filteredWorkOrders = filteredWorkOrders.Where(w =>
                w.WorkOrderNumber.ToLower().Contains(keyword) ||
                w.OrderNumber.ToLower().Contains(keyword) ||
                w.ItemCode.ToLower().Contains(keyword));
        }

        search.StatusOptions = GetStatusSelectList();

        var viewModel = new WorkOrderListViewModel
        {
            WorkOrders = filteredWorkOrders.Select(WorkOrderViewModel.FromDomain).ToList(),
            Search = search
        };

        return View(viewModel);
    }

    /// <summary>
    /// 作業指示詳細
    /// </summary>
    public async Task<IActionResult> Details(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return NotFound();
        }

        try
        {
            var workOrder = await _workOrderUseCase.GetWorkOrderAsync(id);
            var viewModel = WorkOrderViewModel.FromDomain(workOrder);

            // 品目名を取得
            try
            {
                var item = await _itemUseCase.GetItemByCodeAsync(workOrder.ItemCode);
                viewModel.ItemName = item.ItemName;
            }
            catch (ItemNotFoundException)
            {
                // 品目が見つからない場合はコードのみ表示
            }

            return View(viewModel);
        }
        catch (WorkOrderNotFoundException)
        {
            return NotFound();
        }
    }

    /// <summary>
    /// 作業指示登録画面
    /// </summary>
    public async Task<IActionResult> Create()
    {
        var viewModel = new WorkOrderCreateViewModel
        {
            OrderOptions = await GetOrderSelectListAsync()
        };
        return View(viewModel);
    }

    /// <summary>
    /// 作業指示登録処理
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Create(WorkOrderCreateViewModel model)
    {
        if (!ModelState.IsValid)
        {
            model.OrderOptions = await GetOrderSelectListAsync();
            return View(model);
        }

        try
        {
            var command = new WorkOrderCreateCommand
            {
                OrderNumber = model.OrderNumber!,
                WorkOrderDate = model.WorkOrderDate,
                LocationCode = model.LocationCode!,
                PlannedStartDate = model.PlannedStartDate,
                PlannedEndDate = model.PlannedEndDate,
                Remarks = model.Remarks
            };

            var createdWorkOrder = await _workOrderUseCase.CreateWorkOrderAsync(command);
            TempData["Success"] = $"作業指示「{createdWorkOrder.WorkOrderNumber}」を登録しました。";
            return RedirectToAction(nameof(Details), new { id = createdWorkOrder.WorkOrderNumber });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "作業指示登録に失敗しました");
            TempData["Error"] = "作業指示の登録に失敗しました。";
            model.OrderOptions = await GetOrderSelectListAsync();
            return View(model);
        }
    }

    /// <summary>
    /// 作業開始
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Start(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return NotFound();
        }

        try
        {
            await _workOrderUseCase.StartWorkAsync(id);
            TempData["Success"] = $"作業指示「{id}」の作業を開始しました。";
            return RedirectToAction(nameof(Details), new { id });
        }
        catch (WorkOrderNotFoundException)
        {
            return NotFound();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "作業開始に失敗しました: WorkOrderNumber={WorkOrderNumber}", id);
            TempData["Error"] = "作業の開始に失敗しました。";
            return RedirectToAction(nameof(Details), new { id });
        }
    }

    /// <summary>
    /// 作業完了
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Complete(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return NotFound();
        }

        try
        {
            await _workOrderUseCase.CompleteWorkAsync(id);
            TempData["Success"] = $"作業指示「{id}」の作業を完了しました。";
            return RedirectToAction(nameof(Details), new { id });
        }
        catch (WorkOrderNotFoundException)
        {
            return NotFound();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "作業完了に失敗しました: WorkOrderNumber={WorkOrderNumber}", id);
            TempData["Error"] = "作業の完了に失敗しました。";
            return RedirectToAction(nameof(Details), new { id });
        }
    }

    private static List<SelectListItem> GetStatusSelectList()
    {
        return Enum.GetValues<WorkOrderStatus>()
            .Select(s => new SelectListItem
            {
                Value = s.ToString(),
                Text = s.GetDisplayName()
            })
            .ToList();
    }

    private async Task<List<SelectListItem>> GetOrderSelectListAsync()
    {
        var orders = await _orderRepository.FindAllAsync();
        return orders
            .Where(o => o.Status == Domain.Models.Plan.PlanStatus.Confirmed)
            .Select(o => new SelectListItem
            {
                Value = o.OrderNumber,
                Text = $"{o.OrderNumber} - {o.ItemCode} ({o.DueDate:yyyy/MM/dd})"
            })
            .ToList();
    }
}
