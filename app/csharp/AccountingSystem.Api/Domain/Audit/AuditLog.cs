namespace AccountingSystem.Domain.Audit;

/// <summary>
/// 監査ログドメインモデル（不変）
/// </summary>
public record AuditLog
{
    public long? Id { get; init; }
    public required string EntityType { get; init; }
    public required string EntityId { get; init; }
    public required AuditAction Action { get; init; }
    public required string UserId { get; init; }
    public required string UserName { get; init; }
    public required DateTime Timestamp { get; init; }
    public Dictionary<string, object>? OldValues { get; init; }
    public Dictionary<string, object>? NewValues { get; init; }
    public Dictionary<string, object>? Changes { get; init; }
    public string? Reason { get; init; }
    public string? IpAddress { get; init; }
    public string? UserAgent { get; init; }

    /// <summary>
    /// ファクトリメソッド：CREATE 操作用
    /// </summary>
    public static AuditLog Create(
        string entityType,
        string entityId,
        AuditAction action,
        string userId,
        string userName,
        Dictionary<string, object> changes,
        string? ipAddress)
    {
        return new AuditLog
        {
            EntityType = entityType,
            EntityId = entityId,
            Action = action,
            UserId = userId,
            UserName = userName,
            Timestamp = DateTime.UtcNow,
            Changes = changes,
            IpAddress = ipAddress
        };
    }

    /// <summary>
    /// ファクトリメソッド：UPDATE 操作用
    /// </summary>
    public static AuditLog CreateForUpdate(
        string entityType,
        string entityId,
        string userId,
        string userName,
        Dictionary<string, object> oldValues,
        Dictionary<string, object> newValues,
        string? ipAddress)
    {
        return new AuditLog
        {
            EntityType = entityType,
            EntityId = entityId,
            Action = AuditAction.UPDATE,
            UserId = userId,
            UserName = userName,
            Timestamp = DateTime.UtcNow,
            OldValues = oldValues,
            NewValues = newValues,
            IpAddress = ipAddress
        };
    }

    /// <summary>
    /// ファクトリメソッド：DELETE 操作用
    /// </summary>
    public static AuditLog CreateForDelete(
        string entityType,
        string entityId,
        string userId,
        string userName,
        Dictionary<string, object> oldValues,
        string? reason,
        string? ipAddress)
    {
        return new AuditLog
        {
            EntityType = entityType,
            EntityId = entityId,
            Action = AuditAction.DELETE,
            UserId = userId,
            UserName = userName,
            Timestamp = DateTime.UtcNow,
            OldValues = oldValues,
            Reason = reason,
            IpAddress = ipAddress
        };
    }

    /// <summary>
    /// サマリー文字列を生成
    /// </summary>
    public string GetSummary()
    {
        return $"{EntityType} {EntityId} を{Action.GetDisplayName()}";
    }
}
