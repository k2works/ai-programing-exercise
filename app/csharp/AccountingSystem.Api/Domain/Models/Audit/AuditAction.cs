namespace AccountingSystem.Domain.Models.Audit;

/// <summary>
/// 監査ログのアクション種別
/// </summary>
public enum AuditAction
{
    CREATE,
    UPDATE,
    DELETE
}

public static class AuditActionExtensions
{
    public static string GetDisplayName(this AuditAction action)
    {
        return action switch
        {
            AuditAction.CREATE => "作成",
            AuditAction.UPDATE => "更新",
            AuditAction.DELETE => "削除",
            _ => throw new ArgumentException($"Unknown action: {action}")
        };
    }
}
