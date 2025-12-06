namespace AccountingSystem.Application.Port.Out

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Models

/// <summary>
/// 監査ログリポジトリインターフェース（Port/Out）
/// </summary>
type IAuditLogRepository =
    /// <summary>
    /// 監査ログを保存する
    /// </summary>
    abstract member InsertAsync: auditLog: AuditLog -> Task<AuditLog>

    /// <summary>
    /// エンティティ別の監査ログを取得する
    /// </summary>
    abstract member FindByEntityAsync: entityType: string * entityId: string -> Task<AuditLog list>

    /// <summary>
    /// ユーザー別の監査ログを期間指定で取得する
    /// </summary>
    abstract member FindByUserAsync: userId: string * startDate: DateTime * endDate: DateTime -> Task<AuditLog list>

    /// <summary>
    /// 期間内のすべての監査ログを取得する
    /// </summary>
    abstract member FindByDateRangeAsync: startDate: DateTime * endDate: DateTime -> Task<AuditLog list>

    /// <summary>
    /// アクション種別で監査ログを検索する
    /// </summary>
    abstract member FindByActionAsync: action: string * startDate: DateTime * endDate: DateTime -> Task<AuditLog list>
