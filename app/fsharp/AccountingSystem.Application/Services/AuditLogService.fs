namespace AccountingSystem.Application.Services

open System
open System.Threading.Tasks
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Domain.Models

/// <summary>
/// 監査ログサービス実装
/// </summary>
type AuditLogService(repository: IAuditLogRepository) =

    interface IAuditLogUseCase with
        /// <summary>
        /// 監査ログを記録する
        /// </summary>
        member _.RecordAsync(auditLog: AuditLog) : Task<AuditLog> =
            repository.InsertAsync(auditLog)

        /// <summary>
        /// エンティティ別の監査ログを取得する
        /// </summary>
        member _.GetByEntityAsync(entityType: string, entityId: string) : Task<AuditLog list> =
            repository.FindByEntityAsync(entityType, entityId)

        /// <summary>
        /// ユーザー別の監査ログを期間指定で取得する
        /// </summary>
        member _.GetByUserAsync(userId: string, startDate: DateTime, endDate: DateTime) : Task<AuditLog list> =
            repository.FindByUserAsync(userId, startDate, endDate)

        /// <summary>
        /// 期間内のすべての監査ログを取得する
        /// </summary>
        member _.GetByDateRangeAsync(startDate: DateTime, endDate: DateTime) : Task<AuditLog list> =
            repository.FindByDateRangeAsync(startDate, endDate)

        /// <summary>
        /// アクション種別で監査ログを検索する
        /// </summary>
        member _.GetByActionAsync(action: string, startDate: DateTime, endDate: DateTime) : Task<AuditLog list> =
            repository.FindByActionAsync(action, startDate, endDate)
