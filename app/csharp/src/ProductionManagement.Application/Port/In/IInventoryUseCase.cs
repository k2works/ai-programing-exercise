using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Port.In;

/// <summary>
/// 在庫ユースケース（Input Port）
/// </summary>
public interface IInventoryUseCase
{
    /// <summary>
    /// 在庫一覧を取得
    /// </summary>
    Task<IReadOnlyList<Stock>> GetInventoryAsync(InventoryQuery query);

    /// <summary>
    /// 在庫サマリーを取得
    /// </summary>
    Task<IReadOnlyList<InventorySummary>> GetInventorySummaryAsync();

    /// <summary>
    /// 在庫不足品目を取得
    /// </summary>
    Task<IReadOnlyList<InventorySummary>> GetShortageItemsAsync();
}

/// <summary>
/// 在庫照会クエリ
/// </summary>
public record InventoryQuery(
    string? ItemCode = null,
    string? LocationCode = null,
    StockStatus? Status = null
);

/// <summary>
/// 在庫サマリー
/// </summary>
public record InventorySummary(
    string ItemCode,
    string ItemName,
    decimal TotalQuantity,
    decimal? SafetyStock,
    StockState StockState
);

/// <summary>
/// 在庫状態
/// </summary>
public enum StockState
{
    Normal,
    Shortage,
    Excess
}
