using AccountingSystem.Infrastructure.Persistence.Dapper.Entities;
using AccountingSystem.Domain.Models.Audit;

namespace AccountingSystem.Infrastructure.Web.Dtos;

/// <summary>
/// 監査ログレスポンス DTO
/// </summary>
public class AuditLogResponse
{
    public long? Id { get; set; }
    public string EntityType { get; set; } = string.Empty;
    public string EntityId { get; set; } = string.Empty;
    public string Action { get; set; } = string.Empty;
    public string ActionDisplayName { get; set; } = string.Empty;
    public string UserId { get; set; } = string.Empty;
    public string UserName { get; set; } = string.Empty;
    public DateTime Timestamp { get; set; }
    public Dictionary<string, object>? OldValues { get; set; }
    public Dictionary<string, object>? NewValues { get; set; }
    public Dictionary<string, object>? Changes { get; set; }
    public string? Reason { get; set; }
    public string? IpAddress { get; set; }
    public string? Summary { get; set; }

    public static AuditLogResponse From(AuditLog auditLog)
    {
        return new AuditLogResponse
        {
            Id = auditLog.Id,
            EntityType = auditLog.EntityType,
            EntityId = auditLog.EntityId,
            Action = auditLog.Action.ToString(),
            ActionDisplayName = auditLog.Action.GetDisplayName(),
            UserId = auditLog.UserId,
            UserName = auditLog.UserName,
            Timestamp = auditLog.Timestamp,
            OldValues = auditLog.OldValues,
            NewValues = auditLog.NewValues,
            Changes = auditLog.Changes,
            Reason = auditLog.Reason,
            IpAddress = auditLog.IpAddress,
            Summary = auditLog.GetSummary()
        };
    }
}
