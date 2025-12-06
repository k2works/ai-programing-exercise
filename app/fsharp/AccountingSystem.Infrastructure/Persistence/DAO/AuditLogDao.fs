namespace AccountingSystem.Infrastructure.Persistence.DAO

open System

/// <summary>
/// 監査ログ DAO（Database Access Object）
/// Dapper マッピング用の Entity
/// </summary>
[<CLIMutable>]
type AuditLogDao = {
    id: int64
    entity_type: string
    entity_id: string
    action: string
    user_id: string
    user_name: string
    timestamp: DateTime
    old_values: string
    new_values: string
    changes: string
    reason: string
    ip_address: string
    user_agent: string
    created_at: DateTime
}
