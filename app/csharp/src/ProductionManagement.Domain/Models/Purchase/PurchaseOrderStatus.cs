namespace ProductionManagement.Domain.Models.Purchase;

/// <summary>
/// 発注ステータス
/// </summary>
public enum PurchaseOrderStatus
{
    /// <summary>作成中</summary>
    Creating,
    /// <summary>発注済</summary>
    Ordered,
    /// <summary>一部入荷</summary>
    PartiallyReceived,
    /// <summary>入荷完了</summary>
    Received,
    /// <summary>検収完了</summary>
    Accepted,
    /// <summary>取消</summary>
    Cancelled
}

/// <summary>
/// PurchaseOrderStatus 拡張メソッド
/// </summary>
public static class PurchaseOrderStatusExtensions
{
    private static readonly Dictionary<PurchaseOrderStatus, string> DisplayNames = new()
    {
        { PurchaseOrderStatus.Creating, "作成中" },
        { PurchaseOrderStatus.Ordered, "発注済" },
        { PurchaseOrderStatus.PartiallyReceived, "一部入荷" },
        { PurchaseOrderStatus.Received, "入荷完了" },
        { PurchaseOrderStatus.Accepted, "検収完了" },
        { PurchaseOrderStatus.Cancelled, "取消" }
    };

    private static readonly Dictionary<string, PurchaseOrderStatus> FromDisplayNames =
        DisplayNames.ToDictionary(x => x.Value, x => x.Key);

    public static string GetDisplayName(this PurchaseOrderStatus status) => DisplayNames[status];

    public static PurchaseOrderStatus FromDisplayName(string displayName)
    {
        if (FromDisplayNames.TryGetValue(displayName, out var status))
        {
            return status;
        }
        throw new ArgumentException($"Unknown purchase order status: {displayName}");
    }
}
