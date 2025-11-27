using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Entities;
using AccountingSystem.Domain.Events;
using AccountingSystem.Domain.Models.Audit;
using Microsoft.Extensions.Logging;

namespace AccountingSystem.Infrastructure.EventBus;

/// <summary>
/// 監査ログ用イベントバスハンドラー
/// RabbitMQ から受信した仕訳イベントを監査ログに記録
/// </summary>
public class AuditLogJournalEntryCreatedHandler : IEventBusHandler<JournalEntryCreatedEvent>
{
    private readonly IAuditLogRepository _auditLogRepository;
    private readonly ILogger<AuditLogJournalEntryCreatedHandler> _logger;

    public AuditLogJournalEntryCreatedHandler(
        IAuditLogRepository auditLogRepository,
        ILogger<AuditLogJournalEntryCreatedHandler> logger)
    {
        _auditLogRepository = auditLogRepository;
        _logger = logger;
    }

    public async Task HandleAsync(JournalEntryCreatedEvent @event)
    {
        _logger.LogInformation(
            "監査ログ記録: 仕訳作成 JournalEntryId={JournalEntryId}, UserId={UserId}",
            @event.JournalEntryId, @event.UserId);

        var changes = new Dictionary<string, object>
        {
            ["EntryDate"] = @event.EntryDate.ToString(),
            ["Description"] = @event.Description,
            ["LineItemsCount"] = @event.LineItems.Count
        };

        var auditLog = AuditLog.Create(
            entityType: "JournalEntry",
            entityId: @event.JournalEntryId,
            action: AuditAction.CREATE,
            userId: @event.UserId,
            userName: @event.UserId,
            changes: changes,
            ipAddress: null
        );

        await _auditLogRepository.InsertAsync(auditLog);
    }
}

/// <summary>
/// 仕訳承認イベントの監査ログハンドラー
/// </summary>
public class AuditLogJournalEntryApprovedHandler : IEventBusHandler<JournalEntryApprovedEvent>
{
    private readonly IAuditLogRepository _auditLogRepository;
    private readonly ILogger<AuditLogJournalEntryApprovedHandler> _logger;

    public AuditLogJournalEntryApprovedHandler(
        IAuditLogRepository auditLogRepository,
        ILogger<AuditLogJournalEntryApprovedHandler> logger)
    {
        _auditLogRepository = auditLogRepository;
        _logger = logger;
    }

    public async Task HandleAsync(JournalEntryApprovedEvent @event)
    {
        _logger.LogInformation(
            "監査ログ記録: 仕訳承認 JournalEntryId={JournalEntryId}, ApprovedBy={ApprovedBy}",
            @event.JournalEntryId, @event.ApprovedBy);

        var oldValues = new Dictionary<string, object>
        {
            ["Status"] = "PENDING"
        };

        var newValues = new Dictionary<string, object>
        {
            ["Status"] = "APPROVED",
            ["ApprovedBy"] = @event.ApprovedBy,
            ["ApprovalComment"] = @event.ApprovalComment ?? string.Empty
        };

        var auditLog = AuditLog.CreateForUpdate(
            entityType: "JournalEntry",
            entityId: @event.JournalEntryId,
            userId: @event.ApprovedBy,
            userName: @event.ApprovedBy,
            oldValues: oldValues,
            newValues: newValues,
            ipAddress: null
        );

        await _auditLogRepository.InsertAsync(auditLog);
    }
}

/// <summary>
/// 仕訳削除イベントの監査ログハンドラー
/// </summary>
public class AuditLogJournalEntryDeletedHandler : IEventBusHandler<JournalEntryDeletedEvent>
{
    private readonly IAuditLogRepository _auditLogRepository;
    private readonly ILogger<AuditLogJournalEntryDeletedHandler> _logger;

    public AuditLogJournalEntryDeletedHandler(
        IAuditLogRepository auditLogRepository,
        ILogger<AuditLogJournalEntryDeletedHandler> logger)
    {
        _auditLogRepository = auditLogRepository;
        _logger = logger;
    }

    public async Task HandleAsync(JournalEntryDeletedEvent @event)
    {
        _logger.LogInformation(
            "監査ログ記録: 仕訳削除 JournalEntryId={JournalEntryId}, UserId={UserId}",
            @event.JournalEntryId, @event.UserId);

        var oldValues = new Dictionary<string, object>
        {
            ["JournalEntryId"] = @event.JournalEntryId
        };

        var auditLog = AuditLog.CreateForDelete(
            entityType: "JournalEntry",
            entityId: @event.JournalEntryId,
            userId: @event.UserId,
            userName: @event.UserId,
            oldValues: oldValues,
            reason: @event.Reason,
            ipAddress: null
        );

        await _auditLogRepository.InsertAsync(auditLog);
    }
}
