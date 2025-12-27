namespace ProductionManagement.Domain.Models.Process;

/// <summary>
/// 作業指示ステータス
/// </summary>
public enum WorkOrderStatus
{
    NotStarted,
    InProgress,
    Completed,
    Suspended
}

public static class WorkOrderStatusExtensions
{
    private static readonly Dictionary<WorkOrderStatus, string> DisplayNames = new()
    {
        { WorkOrderStatus.NotStarted, "未着手" },
        { WorkOrderStatus.InProgress, "作業中" },
        { WorkOrderStatus.Completed, "完了" },
        { WorkOrderStatus.Suspended, "中断" }
    };

    public static string GetDisplayName(this WorkOrderStatus status)
        => DisplayNames[status];

    public static WorkOrderStatus FromDisplayName(string displayName)
        => DisplayNames.First(x => x.Value == displayName).Key;
}
