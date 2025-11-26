using AccountingSystem.Infrastructure.Persistence.Dapper.Entities;
using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Models.Audit;

namespace AccountingSystem.Application.Services;

/// <summary>
/// 監査ログサービス実装
/// </summary>
public class AuditLogService : IAuditLogService
{
    private readonly IAuditLogRepository _auditLogRepository;

    public AuditLogService(IAuditLogRepository auditLogRepository)
    {
        _auditLogRepository = auditLogRepository;
    }

    public async Task RecordAsync(AuditLog auditLog)
    {
        await _auditLogRepository.InsertAsync(auditLog);
    }

    public async Task<IReadOnlyList<AuditLog>> GetEntityHistoryAsync(
        string entityType,
        string entityId)
    {
        return await _auditLogRepository.FindByEntityAsync(entityType, entityId);
    }

    public async Task<IReadOnlyList<AuditLog>> GetUserActivityAsync(
        string userId,
        DateTime startDate,
        DateTime endDate)
    {
        return await _auditLogRepository.FindByUserAsync(userId, startDate, endDate);
    }

    public async Task<IReadOnlyList<AuditLog>> GetAuditLogsForPeriodAsync(
        DateTime startDate,
        DateTime endDate,
        int limit)
    {
        return await _auditLogRepository.FindByPeriodAsync(startDate, endDate, limit);
    }

    public async Task<IReadOnlyList<AuditLog>> GetAuditLogsByActionAsync(
        AuditAction action,
        DateTime startDate,
        DateTime endDate,
        int limit)
    {
        return await _auditLogRepository.FindByActionAsync(action, startDate, endDate, limit);
    }
}
