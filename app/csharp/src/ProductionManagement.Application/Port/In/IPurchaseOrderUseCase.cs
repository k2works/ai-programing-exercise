using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Port.In;

/// <summary>
/// 発注ユースケース（Input Port）
/// </summary>
public interface IPurchaseOrderUseCase
{
    /// <summary>
    /// 発注を作成する
    /// </summary>
    Task<PurchaseOrder> CreateOrderAsync(PurchaseOrderCreateCommand command);

    /// <summary>
    /// 発注を取得する
    /// </summary>
    Task<PurchaseOrder> GetOrderAsync(string purchaseOrderNumber);

    /// <summary>
    /// 発注一覧を取得する
    /// </summary>
    Task<IReadOnlyList<PurchaseOrder>> GetAllOrdersAsync();

    /// <summary>
    /// 発注を確定する
    /// </summary>
    Task<PurchaseOrder> ConfirmOrderAsync(string purchaseOrderNumber);

    /// <summary>
    /// 発注を取消する
    /// </summary>
    Task CancelOrderAsync(string purchaseOrderNumber);
}
