namespace AccountingSystem.Application.Port.In

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Models

/// <summary>
/// 監査ログユースケースインターフェース（Port/In）
/// </summary>
type IAuditLogUseCase =
    /// <summary>
    /// 監査ログを記録する
    /// </summary>
    abstract member RecordAsync: auditLog: AuditLog -> Task<AuditLog>

    /// <summary>
    /// エンティティ別の監査ログを取得する
    /// </summary>
    abstract member GetByEntityAsync: entityType: string * entityId: string -> Task<AuditLog list>

    /// <summary>
    /// ユーザー別の監査ログを期間指定で取得する
    /// </summary>
    abstract member GetByUserAsync: userId: string * startDate: DateTime * endDate: DateTime -> Task<AuditLog list>

    /// <summary>
    /// 期間内のすべての監査ログを取得する
    /// </summary>
    abstract member GetByDateRangeAsync: startDate: DateTime * endDate: DateTime -> Task<AuditLog list>

    /// <summary>
    /// アクション種別で監査ログを検索する
    /// </summary>
    abstract member GetByActionAsync: action: string * startDate: DateTime * endDate: DateTime -> Task<AuditLog list>
