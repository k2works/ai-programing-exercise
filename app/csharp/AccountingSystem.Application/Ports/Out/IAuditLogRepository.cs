using AccountingSystem.Domain.Entities;
using AccountingSystem.Domain.Models.Audit;

namespace AccountingSystem.Application.Ports.Out;

/// <summary>
/// 監査ログリポジトリインターフェース（Output Port）
/// </summary>
public interface IAuditLogRepository
{
    /// <summary>
    /// 監査ログを登録（Append-Only）
    /// </summary>
    Task<AuditLog> InsertAsync(AuditLog auditLog);

    /// <summary>
    /// エンティティの変更履歴を取得
    /// </summary>
    Task<IReadOnlyList<AuditLog>> FindByEntityAsync(string entityType, string entityId);

    /// <summary>
    /// ユーザーの操作履歴を取得
    /// </summary>
    Task<IReadOnlyList<AuditLog>> FindByUserAsync(string userId, DateTime startDate, DateTime endDate);

    /// <summary>
    /// 期間別の監査ログを取得
    /// </summary>
    Task<IReadOnlyList<AuditLog>> FindByPeriodAsync(DateTime startDate, DateTime endDate, int limit);

    /// <summary>
    /// アクション別の監査ログを取得
    /// </summary>
    Task<IReadOnlyList<AuditLog>> FindByActionAsync(AuditAction action, DateTime startDate, DateTime endDate, int limit);
}
