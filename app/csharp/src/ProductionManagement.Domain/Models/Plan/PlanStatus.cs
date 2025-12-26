namespace ProductionManagement.Domain.Models.Plan;

/// <summary>
/// 計画ステータス
/// </summary>
public enum PlanStatus
{
    /// <summary>草案</summary>
    Draft,
    /// <summary>確定</summary>
    Confirmed,
    /// <summary>展開済</summary>
    Expanded,
    /// <summary>取消</summary>
    Cancelled
}

/// <summary>
/// PlanStatus 拡張メソッド
/// </summary>
public static class PlanStatusExtensions
{
    private static readonly Dictionary<PlanStatus, string> DisplayNames = new()
    {
        { PlanStatus.Draft, "草案" },
        { PlanStatus.Confirmed, "確定" },
        { PlanStatus.Expanded, "展開済" },
        { PlanStatus.Cancelled, "取消" }
    };

    private static readonly Dictionary<string, PlanStatus> FromDisplayNames =
        DisplayNames.ToDictionary(x => x.Value, x => x.Key);

    public static string GetDisplayName(this PlanStatus status) => DisplayNames[status];

    public static PlanStatus FromDisplayName(string displayName)
    {
        if (FromDisplayNames.TryGetValue(displayName, out var status))
        {
            return status;
        }
        throw new ArgumentException($"Unknown plan status: {displayName}");
    }
}
