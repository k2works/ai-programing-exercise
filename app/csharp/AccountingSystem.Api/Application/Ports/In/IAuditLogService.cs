using AccountingSystem.Infrastructure.Persistence.Dapper.Entities;
using AccountingSystem.Domain.Audit;

namespace AccountingSystem.Application.Ports.In;

/// <summary>
/// 監査ログサービスインターフェース（Input Port）
/// </summary>
public interface IAuditLogService
{
    /// <summary>
    /// 監査ログを記録
    /// </summary>
    Task RecordAsync(AuditLog auditLog);

    /// <summary>
    /// エンティティの変更履歴を取得
    /// </summary>
    Task<IReadOnlyList<AuditLog>> GetEntityHistoryAsync(string entityType, string entityId);

    /// <summary>
    /// ユーザーの操作履歴を取得
    /// </summary>
    Task<IReadOnlyList<AuditLog>> GetUserActivityAsync(string userId, DateTime startDate, DateTime endDate);

    /// <summary>
    /// 期間別の監査ログを取得
    /// </summary>
    Task<IReadOnlyList<AuditLog>> GetAuditLogsForPeriodAsync(DateTime startDate, DateTime endDate, int limit);

    /// <summary>
    /// アクション別の監査ログを取得
    /// </summary>
    Task<IReadOnlyList<AuditLog>> GetAuditLogsByActionAsync(AuditAction action, DateTime startDate, DateTime endDate, int limit);
}
