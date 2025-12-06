namespace AccountingSystem.Infrastructure.Persistence.Repositories

open System
open System.Threading.Tasks
open Dapper
open Npgsql
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Models
open AccountingSystem.Infrastructure.Persistence.DAO

/// <summary>
/// 監査ログリポジトリ実装
/// </summary>
type AuditLogRepository(connection: NpgsqlConnection) =

    /// DAO → Domain Model 変換
    let toDomain (dao: AuditLogDao) : AuditLog option =
        AuditAction.fromCode dao.action
        |> Option.map (fun action ->
            {
                Id = Some dao.id
                EntityType = dao.entity_type
                EntityId = dao.entity_id
                Action = action
                UserId = dao.user_id
                UserName = dao.user_name
                Timestamp = dao.timestamp
                OldValues = if isNull dao.old_values then None else Some dao.old_values
                NewValues = if isNull dao.new_values then None else Some dao.new_values
                Changes = if isNull dao.changes then None else Some dao.changes
                Reason = if isNull dao.reason then None else Some dao.reason
                IpAddress = if isNull dao.ip_address then None else Some dao.ip_address
                UserAgent = if isNull dao.user_agent then None else Some dao.user_agent
            })

    /// <summary>
    /// 監査ログを保存する
    /// </summary>
    member _.InsertAsync(auditLog: AuditLog) : Task<AuditLog> =
        task {
            let sql = """
                INSERT INTO audit_log (
                    entity_type, entity_id, action, user_id, user_name,
                    timestamp, old_values, new_values, changes, reason,
                    ip_address, user_agent
                ) VALUES (
                    @EntityType, @EntityId, @Action, @UserId, @UserName,
                    @Timestamp, @OldValues::jsonb, @NewValues::jsonb,
                    @Changes::jsonb, @Reason, @IpAddress, @UserAgent
                )
                RETURNING id
            """

            let! id = connection.ExecuteScalarAsync<int64>(sql, {|
                EntityType = auditLog.EntityType
                EntityId = auditLog.EntityId
                Action = AuditAction.toCode auditLog.Action
                UserId = auditLog.UserId
                UserName = auditLog.UserName
                Timestamp = auditLog.Timestamp
                OldValues = auditLog.OldValues |> Option.toObj
                NewValues = auditLog.NewValues |> Option.toObj
                Changes = auditLog.Changes |> Option.toObj
                Reason = auditLog.Reason |> Option.toObj
                IpAddress = auditLog.IpAddress |> Option.toObj
                UserAgent = auditLog.UserAgent |> Option.toObj
            |})

            return { auditLog with Id = Some id }
        }

    /// <summary>
    /// エンティティ別の監査ログを取得する
    /// </summary>
    member _.FindByEntityAsync(entityType: string, entityId: string) : Task<AuditLog list> =
        task {
            let sql = """
                SELECT * FROM audit_log
                WHERE entity_type = @EntityType
                  AND entity_id = @EntityId
                ORDER BY timestamp DESC
            """

            let! daos = connection.QueryAsync<AuditLogDao>(sql, {|
                EntityType = entityType
                EntityId = entityId
            |})

            return
                daos
                |> Seq.choose toDomain
                |> Seq.toList
        }

    /// <summary>
    /// ユーザー別の監査ログを期間指定で取得する
    /// </summary>
    member _.FindByUserAsync(userId: string, startDate: DateTime, endDate: DateTime) : Task<AuditLog list> =
        task {
            let sql = """
                SELECT * FROM audit_log
                WHERE user_id = @UserId
                  AND timestamp BETWEEN @StartDate AND @EndDate
                ORDER BY timestamp DESC
            """

            let! daos = connection.QueryAsync<AuditLogDao>(sql, {|
                UserId = userId
                StartDate = startDate
                EndDate = endDate
            |})

            return
                daos
                |> Seq.choose toDomain
                |> Seq.toList
        }

    /// <summary>
    /// 期間内のすべての監査ログを取得する
    /// </summary>
    member _.FindByDateRangeAsync(startDate: DateTime, endDate: DateTime) : Task<AuditLog list> =
        task {
            let sql = """
                SELECT * FROM audit_log
                WHERE timestamp BETWEEN @StartDate AND @EndDate
                ORDER BY timestamp DESC
            """

            let! daos = connection.QueryAsync<AuditLogDao>(sql, {|
                StartDate = startDate
                EndDate = endDate
            |})

            return
                daos
                |> Seq.choose toDomain
                |> Seq.toList
        }

    /// <summary>
    /// アクション種別で監査ログを検索する
    /// </summary>
    member _.FindByActionAsync(action: string, startDate: DateTime, endDate: DateTime) : Task<AuditLog list> =
        task {
            let sql = """
                SELECT * FROM audit_log
                WHERE action = @Action
                  AND timestamp BETWEEN @StartDate AND @EndDate
                ORDER BY timestamp DESC
            """

            let! daos = connection.QueryAsync<AuditLogDao>(sql, {|
                Action = action
                StartDate = startDate
                EndDate = endDate
            |})

            return
                daos
                |> Seq.choose toDomain
                |> Seq.toList
        }
