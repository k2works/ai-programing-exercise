namespace AccountingSystem.Infrastructure.Web.Dtos

open System
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Models

/// <summary>
/// 監査ログレスポンスDTO
/// </summary>
[<CLIMutable>]
type AuditLogResponse = {
    Id: int64
    EntityType: string
    EntityId: string
    Action: string
    ActionDisplayName: string
    UserId: string
    UserName: string
    Timestamp: DateTime
    OldValues: string option
    NewValues: string option
    Changes: string option
    Reason: string option
    IpAddress: string option
    Summary: string
}

module AuditLogResponse =
    /// Domain Model → Response DTO 変換
    let fromDomain (auditLog: AuditLog) : AuditLogResponse =
        {
            Id = auditLog.Id |> Option.defaultValue 0L
            EntityType = auditLog.EntityType
            EntityId = auditLog.EntityId
            Action = AuditAction.toCode auditLog.Action
            ActionDisplayName = AuditAction.getDisplayName auditLog.Action
            UserId = auditLog.UserId
            UserName = auditLog.UserName
            Timestamp = auditLog.Timestamp
            OldValues = auditLog.OldValues
            NewValues = auditLog.NewValues
            Changes = auditLog.Changes
            Reason = auditLog.Reason
            IpAddress = auditLog.IpAddress
            Summary = AuditLog.getSummary auditLog
        }

/// <summary>
/// 監査ログ作成リクエストDTO
/// </summary>
[<CLIMutable>]
type CreateAuditLogRequest = {
    EntityType: string
    EntityId: string
    Action: string
    UserId: string
    UserName: string
    OldValues: string option
    NewValues: string option
    Changes: string option
    Reason: string option
    IpAddress: string option
}

module CreateAuditLogRequest =
    /// Request DTO → Domain Model 変換
    let toDomain (request: CreateAuditLogRequest) : AuditLog option =
        AuditAction.fromCode request.Action
        |> Option.map (fun action ->
            {
                Id = None
                EntityType = request.EntityType
                EntityId = request.EntityId
                Action = action
                UserId = request.UserId
                UserName = request.UserName
                Timestamp = DateTime.UtcNow
                OldValues = request.OldValues
                NewValues = request.NewValues
                Changes = request.Changes
                Reason = request.Reason
                IpAddress = request.IpAddress
                UserAgent = None
            })
