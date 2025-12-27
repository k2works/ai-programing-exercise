using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Web.Models;

namespace ProductionManagement.Web.Controllers;

/// <summary>
/// オーダ管理 Controller
/// </summary>
public class OrdersController : Controller
{
    private readonly IOrderUseCase _orderUseCase;
    private readonly IOrderRepository _orderRepository;
    private readonly IItemUseCase _itemUseCase;
    private readonly ILogger<OrdersController> _logger;

    public OrdersController(
        IOrderUseCase orderUseCase,
        IOrderRepository orderRepository,
        IItemUseCase itemUseCase,
        ILogger<OrdersController> logger)
    {
        _orderUseCase = orderUseCase;
        _orderRepository = orderRepository;
        _itemUseCase = itemUseCase;
        _logger = logger;
    }

    /// <summary>
    /// オーダ一覧
    /// </summary>
    public async Task<IActionResult> Index(OrderSearchViewModel search)
    {
        var allOrders = await _orderRepository.FindAllAsync();

        // フィルタリング
        var filteredOrders = allOrders.AsEnumerable();

        if (!string.IsNullOrEmpty(search.OrderType))
        {
            var orderType = Enum.Parse<OrderType>(search.OrderType);
            filteredOrders = filteredOrders.Where(o => o.OrderType == orderType);
        }

        if (!string.IsNullOrEmpty(search.Status))
        {
            var status = Enum.Parse<PlanStatus>(search.Status);
            filteredOrders = filteredOrders.Where(o => o.Status == status);
        }

        if (search.DueDateFrom.HasValue)
        {
            filteredOrders = filteredOrders.Where(o => o.DueDate >= search.DueDateFrom.Value);
        }

        if (search.DueDateTo.HasValue)
        {
            filteredOrders = filteredOrders.Where(o => o.DueDate <= search.DueDateTo.Value);
        }

        if (!string.IsNullOrEmpty(search.Keyword))
        {
            var keyword = search.Keyword.ToLower();
            filteredOrders = filteredOrders.Where(o =>
                o.OrderNumber.ToLower().Contains(keyword) ||
                o.ItemCode.ToLower().Contains(keyword));
        }

        search.OrderTypeOptions = GetOrderTypeSelectList();
        search.StatusOptions = GetStatusSelectList();

        var viewModel = new OrderListViewModel
        {
            Orders = filteredOrders.Select(OrderViewModel.FromDomain).ToList(),
            Search = search
        };

        return View(viewModel);
    }

    /// <summary>
    /// オーダ詳細
    /// </summary>
    public async Task<IActionResult> Details(int id)
    {
        try
        {
            var order = await _orderUseCase.GetOrderByIdAsync(id);
            var viewModel = OrderViewModel.FromDomain(order);

            // 品目名を取得
            try
            {
                var item = await _itemUseCase.GetItemByCodeAsync(order.ItemCode);
                viewModel.ItemName = item.ItemName;
            }
            catch (ItemNotFoundException)
            {
                // 品目が見つからない場合はコードのみ表示
            }

            return View(viewModel);
        }
        catch (OrderNotFoundException)
        {
            return NotFound();
        }
    }

    /// <summary>
    /// オーダ登録画面
    /// </summary>
    public async Task<IActionResult> Create()
    {
        var viewModel = new OrderCreateViewModel
        {
            OrderTypeOptions = GetOrderTypeSelectList(),
            ItemOptions = await GetItemSelectListAsync()
        };
        return View(viewModel);
    }

    /// <summary>
    /// オーダ登録処理
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Create(OrderCreateViewModel model)
    {
        if (!ModelState.IsValid)
        {
            model.OrderTypeOptions = GetOrderTypeSelectList();
            model.ItemOptions = await GetItemSelectListAsync();
            return View(model);
        }

        try
        {
            var command = new CreateOrderCommand(
                OrderType: model.OrderType,
                ItemCode: model.ItemCode!,
                StartDate: model.StartDate,
                DueDate: model.DueDate,
                PlanQuantity: model.PlanQuantity,
                LocationCode: model.LocationCode!,
                ExpirationDate: model.ExpirationDate
            );

            var createdOrder = await _orderUseCase.CreateOrderAsync(command);
            TempData["Success"] = $"オーダ「{createdOrder.OrderNumber}」を登録しました。";
            return RedirectToAction(nameof(Details), new { id = createdOrder.Id });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "オーダ登録に失敗しました");
            TempData["Error"] = "オーダの登録に失敗しました。";
            model.OrderTypeOptions = GetOrderTypeSelectList();
            model.ItemOptions = await GetItemSelectListAsync();
            return View(model);
        }
    }

    /// <summary>
    /// オーダ確定
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Confirm(int id)
    {
        try
        {
            await _orderUseCase.ConfirmOrderAsync(id);
            TempData["Success"] = "オーダを確定しました。";
            return RedirectToAction(nameof(Details), new { id });
        }
        catch (OrderNotFoundException)
        {
            return NotFound();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "オーダ確定に失敗しました: OrderId={OrderId}", id);
            TempData["Error"] = "オーダの確定に失敗しました。";
            return RedirectToAction(nameof(Details), new { id });
        }
    }

    /// <summary>
    /// オーダ取消
    /// </summary>
    [HttpPost]
    [ValidateAntiForgeryToken]
    public async Task<IActionResult> Cancel(int id)
    {
        try
        {
            await _orderUseCase.CancelOrderAsync(id);
            TempData["Success"] = "オーダを取消しました。";
            return RedirectToAction(nameof(Index));
        }
        catch (OrderNotFoundException)
        {
            return NotFound();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "オーダ取消に失敗しました: OrderId={OrderId}", id);
            TempData["Error"] = "オーダの取消に失敗しました。";
            return RedirectToAction(nameof(Details), new { id });
        }
    }

    private static List<SelectListItem> GetOrderTypeSelectList()
    {
        return Enum.GetValues<OrderType>()
            .Select(t => new SelectListItem
            {
                Value = t.ToString(),
                Text = t.GetDisplayName()
            })
            .ToList();
    }

    private static List<SelectListItem> GetStatusSelectList()
    {
        return Enum.GetValues<PlanStatus>()
            .Select(s => new SelectListItem
            {
                Value = s.ToString(),
                Text = s.GetDisplayName()
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
