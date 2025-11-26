using AccountingSystem.Infrastructure.Persistence.Dapper.Entities;
using AccountingSystem.Application.Ports.In;
using AccountingSystem.Domain.Audit;
using AccountingSystem.Domain.Events;
using MediatR;
using Microsoft.Extensions.Logging;

namespace AccountingSystem.Application.EventHandlers;

/// <summary>
/// 監査ログイベントハンドラー
/// ドメインイベントを受信して自動的に監査ログを記録
/// </summary>
public class AuditEventHandler :
    INotificationHandler<AccountCreatedEvent>,
    INotificationHandler<AccountUpdatedEvent>,
    INotificationHandler<AccountDeletedEvent>,
    INotificationHandler<JournalCreatedEvent>,
    INotificationHandler<JournalDeletedEvent>
{
    private readonly IAuditLogService _auditLogService;
    private readonly ILogger<AuditEventHandler> _logger;

    public AuditEventHandler(
        IAuditLogService auditLogService,
        ILogger<AuditEventHandler> logger)
    {
        _auditLogService = auditLogService;
        _logger = logger;
    }

    /// <summary>
    /// 勘定科目作成イベントの処理
    /// </summary>
    public async Task Handle(
        AccountCreatedEvent notification,
        CancellationToken cancellationToken)
    {
        _logger.LogInformation(
            "Handling AccountCreatedEvent: {AccountCode}",
            notification.AccountCode);

        var auditLog = AuditLog.Create(
            "Account",
            notification.AccountCode,
            AuditAction.CREATE,
            notification.UserId,
            notification.UserName,
            notification.AccountData,
            notification.IpAddress
        );

        await _auditLogService.RecordAsync(auditLog);
    }

    /// <summary>
    /// 勘定科目更新イベントの処理
    /// </summary>
    public async Task Handle(
        AccountUpdatedEvent notification,
        CancellationToken cancellationToken)
    {
        _logger.LogInformation(
            "Handling AccountUpdatedEvent: {AccountCode}",
            notification.AccountCode);

        var auditLog = AuditLog.CreateForUpdate(
            "Account",
            notification.AccountCode,
            notification.UserId,
            notification.UserName,
            notification.OldValues,
            notification.NewValues,
            notification.IpAddress
        );

        await _auditLogService.RecordAsync(auditLog);
    }

    /// <summary>
    /// 勘定科目削除イベントの処理
    /// </summary>
    public async Task Handle(
        AccountDeletedEvent notification,
        CancellationToken cancellationToken)
    {
        _logger.LogInformation(
            "Handling AccountDeletedEvent: {AccountCode}",
            notification.AccountCode);

        var auditLog = AuditLog.CreateForDelete(
            "Account",
            notification.AccountCode,
            notification.UserId,
            notification.UserName,
            notification.DeletedData,
            notification.Reason,
            notification.IpAddress
        );

        await _auditLogService.RecordAsync(auditLog);
    }

    /// <summary>
    /// 仕訳作成イベントの処理
    /// </summary>
    public async Task Handle(
        JournalCreatedEvent notification,
        CancellationToken cancellationToken)
    {
        _logger.LogInformation(
            "Handling JournalCreatedEvent: {JournalNo}",
            notification.JournalNo);

        var auditLog = AuditLog.Create(
            "Journal",
            notification.JournalNo,
            AuditAction.CREATE,
            notification.UserId,
            notification.UserName,
            notification.JournalData,
            notification.IpAddress
        );

        await _auditLogService.RecordAsync(auditLog);
    }

    /// <summary>
    /// 仕訳削除イベントの処理
    /// </summary>
    public async Task Handle(
        JournalDeletedEvent notification,
        CancellationToken cancellationToken)
    {
        _logger.LogInformation(
            "Handling JournalDeletedEvent: {JournalNo}",
            notification.JournalNo);

        var auditLog = AuditLog.CreateForDelete(
            "Journal",
            notification.JournalNo,
            notification.UserId,
            notification.UserName,
            notification.DeletedData,
            notification.Reason,
            notification.IpAddress
        );

        await _auditLogService.RecordAsync(auditLog);
    }
}
