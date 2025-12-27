using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Application.Port.In;

/// <summary>
/// オーダユースケース（Input Port）
/// </summary>
public interface IOrderUseCase
{
    /// <summary>
    /// オーダを登録する
    /// </summary>
    Task<Order> CreateOrderAsync(CreateOrderCommand command);

    /// <summary>
    /// オーダを取得する（ID指定）
    /// </summary>
    Task<Order> GetOrderByIdAsync(int orderId);

    /// <summary>
    /// オーダを取得する（オーダ番号指定）
    /// </summary>
    Task<Order> GetOrderByNumberAsync(string orderNumber);

    /// <summary>
    /// MPSに紐づくオーダを取得する
    /// </summary>
    Task<IReadOnlyList<Order>> GetOrdersByMpsIdAsync(int mpsId);

    /// <summary>
    /// オーダのステータスを更新する
    /// </summary>
    Task<Order> UpdateOrderStatusAsync(int orderId, PlanStatus status);

    /// <summary>
    /// オーダを確定する
    /// </summary>
    Task<Order> ConfirmOrderAsync(int orderId);

    /// <summary>
    /// オーダをキャンセルする
    /// </summary>
    Task CancelOrderAsync(int orderId);
}

/// <summary>
/// オーダ登録コマンド
/// </summary>
public record CreateOrderCommand(
    OrderType OrderType,
    string ItemCode,
    DateOnly StartDate,
    DateOnly DueDate,
    decimal PlanQuantity,
    string LocationCode,
    int? MpsId = null,
    int? ParentOrderId = null,
    DateOnly? ExpirationDate = null
);
