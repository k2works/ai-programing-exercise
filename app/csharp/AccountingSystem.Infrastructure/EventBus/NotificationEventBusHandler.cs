using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Events;
using Microsoft.Extensions.Logging;

namespace AccountingSystem.Infrastructure.EventBus;

/// <summary>
/// 通知用イベントバスハンドラー
/// RabbitMQ から受信した仕訳イベントをメール通知
/// </summary>
public class NotificationJournalEntryCreatedHandler : IEventBusHandler<JournalEntryCreatedEvent>
{
    private readonly IEmailService _emailService;
    private readonly ILogger<NotificationJournalEntryCreatedHandler> _logger;

    public NotificationJournalEntryCreatedHandler(
        IEmailService emailService,
        ILogger<NotificationJournalEntryCreatedHandler> logger)
    {
        _emailService = emailService;
        _logger = logger;
    }

    public async Task HandleAsync(JournalEntryCreatedEvent @event)
    {
        _logger.LogInformation(
            "通知送信: 仕訳作成 JournalEntryId={JournalEntryId}",
            @event.JournalEntryId);

        await _emailService.SendAsync(
            recipient: @event.UserId,
            subject: "仕訳が作成されました",
            body: $@"仕訳 {@event.JournalEntryId} が作成されました。

日付: {@event.EntryDate}
摘要: {@event.Description}
明細数: {@event.LineItems.Count}

作成者: {@event.UserId}
作成日時: {@event.OccurredAt:yyyy/MM/dd HH:mm:ss}"
        );
    }
}

/// <summary>
/// 仕訳承認イベントの通知ハンドラー
/// </summary>
public class NotificationJournalEntryApprovedHandler : IEventBusHandler<JournalEntryApprovedEvent>
{
    private readonly IEmailService _emailService;
    private readonly ILogger<NotificationJournalEntryApprovedHandler> _logger;

    public NotificationJournalEntryApprovedHandler(
        IEmailService emailService,
        ILogger<NotificationJournalEntryApprovedHandler> logger)
    {
        _emailService = emailService;
        _logger = logger;
    }

    public async Task HandleAsync(JournalEntryApprovedEvent @event)
    {
        _logger.LogInformation(
            "通知送信: 仕訳承認 JournalEntryId={JournalEntryId}",
            @event.JournalEntryId);

        await _emailService.SendAsync(
            recipient: @event.ApprovedBy,
            subject: "仕訳が承認されました",
            body: $@"仕訳 {@event.JournalEntryId} が承認されました。

承認者: {@event.ApprovedBy}
コメント: {@event.ApprovalComment}
承認日時: {@event.OccurredAt:yyyy/MM/dd HH:mm:ss}"
        );
    }
}

/// <summary>
/// 仕訳削除イベントの通知ハンドラー
/// </summary>
public class NotificationJournalEntryDeletedHandler : IEventBusHandler<JournalEntryDeletedEvent>
{
    private readonly IEmailService _emailService;
    private readonly ILogger<NotificationJournalEntryDeletedHandler> _logger;

    public NotificationJournalEntryDeletedHandler(
        IEmailService emailService,
        ILogger<NotificationJournalEntryDeletedHandler> logger)
    {
        _emailService = emailService;
        _logger = logger;
    }

    public async Task HandleAsync(JournalEntryDeletedEvent @event)
    {
        _logger.LogInformation(
            "通知送信: 仕訳削除 JournalEntryId={JournalEntryId}",
            @event.JournalEntryId);

        await _emailService.SendAsync(
            recipient: @event.UserId,
            subject: "仕訳が削除されました",
            body: $@"仕訳 {@event.JournalEntryId} が削除されました。

削除理由: {@event.Reason}
削除者: {@event.UserId}
削除日時: {@event.OccurredAt:yyyy/MM/dd HH:mm:ss}"
        );
    }
}
