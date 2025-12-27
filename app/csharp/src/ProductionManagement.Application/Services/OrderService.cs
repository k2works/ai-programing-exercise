using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Application.Services;

/// <summary>
/// オーダアプリケーションサービス
/// </summary>
public class OrderService : IOrderUseCase
{
    private readonly IOrderRepository _orderRepository;
    private readonly IItemRepository _itemRepository;

    public OrderService(IOrderRepository orderRepository, IItemRepository itemRepository)
    {
        _orderRepository = orderRepository;
        _itemRepository = itemRepository;
    }

    public async Task<Order> CreateOrderAsync(CreateOrderCommand command)
    {
        // 品目の存在確認
        var item = await _itemRepository.FindByItemCodeAsync(command.ItemCode)
            ?? throw new ItemNotFoundException(command.ItemCode);

        // オーダ番号を生成
        var orderPrefix = command.OrderType == OrderType.Purchase ? "PO" : "MO";
        var orderNumber = $"{orderPrefix}-{DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()}";

        var order = new Order
        {
            OrderNumber = orderNumber,
            OrderType = command.OrderType,
            ItemCode = command.ItemCode,
            StartDate = command.StartDate,
            DueDate = command.DueDate,
            ExpirationDate = command.ExpirationDate,
            PlanQuantity = command.PlanQuantity,
            LocationCode = command.LocationCode,
            Status = PlanStatus.Draft,
            MpsId = command.MpsId,
            ParentOrderId = command.ParentOrderId
        };

        await _orderRepository.SaveAsync(order);
        return order;
    }

    public async Task<Order> GetOrderByIdAsync(int orderId)
    {
        return await _orderRepository.FindByIdAsync(orderId)
            ?? throw new OrderNotFoundException(orderId);
    }

    public async Task<Order> GetOrderByNumberAsync(string orderNumber)
    {
        return await _orderRepository.FindByOrderNumberAsync(orderNumber)
            ?? throw new OrderNotFoundException(orderNumber);
    }

    public async Task<IReadOnlyList<Order>> GetOrdersByMpsIdAsync(int mpsId)
    {
        return await _orderRepository.FindByMpsIdAsync(mpsId);
    }

    public async Task<Order> UpdateOrderStatusAsync(int orderId, PlanStatus status)
    {
        var order = await _orderRepository.FindByIdAsync(orderId)
            ?? throw new OrderNotFoundException(orderId);

        await _orderRepository.UpdateStatusAsync(orderId, status);
        order.Status = status;
        return order;
    }

    public async Task<Order> ConfirmOrderAsync(int orderId)
    {
        var order = await _orderRepository.FindByIdAsync(orderId)
            ?? throw new OrderNotFoundException(orderId);

        if (order.Status != PlanStatus.Draft)
        {
            throw new InvalidOperationException($"草案状態のオーダのみ確定できます: 現在のステータス={order.Status.GetDisplayName()}");
        }

        await _orderRepository.UpdateStatusAsync(orderId, PlanStatus.Confirmed);
        order.Status = PlanStatus.Confirmed;
        return order;
    }

    public async Task CancelOrderAsync(int orderId)
    {
        var order = await _orderRepository.FindByIdAsync(orderId)
            ?? throw new OrderNotFoundException(orderId);

        if (order.Status == PlanStatus.Expanded)
        {
            throw new InvalidOperationException("展開済みのオーダはキャンセルできません");
        }

        await _orderRepository.UpdateStatusAsync(orderId, PlanStatus.Cancelled);
    }
}
