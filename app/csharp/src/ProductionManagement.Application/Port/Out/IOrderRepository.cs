using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// オーダ情報リポジトリ（Output Port）
/// </summary>
public interface IOrderRepository
{
    Task SaveAsync(Order order);
    Task<Order?> FindByIdAsync(int id);
    Task<Order?> FindByOrderNumberAsync(string orderNumber);
    Task<IReadOnlyList<Order>> FindByMpsIdAsync(int mpsId);
    Task<IReadOnlyList<Order>> FindByParentOrderIdAsync(int parentOrderId);
    Task UpdateParentOrderIdAsync(int id, int parentOrderId);
    Task UpdateStatusAsync(int id, PlanStatus status);
    Task DeleteAllAsync();
}
