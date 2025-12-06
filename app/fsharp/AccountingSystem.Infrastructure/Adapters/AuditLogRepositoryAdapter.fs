namespace AccountingSystem.Infrastructure.Adapters

open System
open System.Threading.Tasks
open Npgsql
open AccountingSystem.Application.Port.Out
open AccountingSystem.Domain.Models
open AccountingSystem.Infrastructure.Persistence.Repositories

/// <summary>
/// 監査ログリポジトリアダプター（Port/Out の実装）
/// 接続文字列を受け取り、内部でコネクションを管理
/// </summary>
type AuditLogRepositoryAdapter(connectionString: string) =

    interface IAuditLogRepository with
        member _.InsertAsync(auditLog: AuditLog) : Task<AuditLog> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = AuditLogRepository(connection)
                return! repository.InsertAsync(auditLog)
            }

        member _.FindByEntityAsync(entityType: string, entityId: string) : Task<AuditLog list> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = AuditLogRepository(connection)
                return! repository.FindByEntityAsync(entityType, entityId)
            }

        member _.FindByUserAsync(userId: string, startDate: DateTime, endDate: DateTime) : Task<AuditLog list> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = AuditLogRepository(connection)
                return! repository.FindByUserAsync(userId, startDate, endDate)
            }

        member _.FindByDateRangeAsync(startDate: DateTime, endDate: DateTime) : Task<AuditLog list> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = AuditLogRepository(connection)
                return! repository.FindByDateRangeAsync(startDate, endDate)
            }

        member _.FindByActionAsync(action: string, startDate: DateTime, endDate: DateTime) : Task<AuditLog list> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = AuditLogRepository(connection)
                return! repository.FindByActionAsync(action, startDate, endDate)
            }
