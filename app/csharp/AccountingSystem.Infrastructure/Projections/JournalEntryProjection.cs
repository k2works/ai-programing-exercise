namespace AccountingSystem.Infrastructure.Projections;

using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Events;
using MediatR;
using Microsoft.Extensions.Logging;

/// <summary>
/// 仕訳 Projection ハンドラー
/// イベントストアから Read Model を構築
/// </summary>
public class JournalEntryProjection :
    INotificationHandler<JournalEntryCreatedEvent>,
    INotificationHandler<JournalEntryApprovedEvent>,
    INotificationHandler<JournalEntryDeletedEvent>
{
    private readonly IJournalEntryReadModelRepository _readModelRepository;
    private readonly ILogger<JournalEntryProjection> _logger;

    public JournalEntryProjection(
        IJournalEntryReadModelRepository readModelRepository,
        ILogger<JournalEntryProjection> logger)
    {
        _readModelRepository = readModelRepository;
        _logger = logger;
    }

    public async Task Handle(JournalEntryCreatedEvent notification, CancellationToken cancellationToken)
    {
        _logger.LogInformation("Projecting JournalEntryCreatedEvent: {JournalEntryId}",
            notification.JournalEntryId);

        // Read Model に保存
        await _readModelRepository.InsertJournalEntryAsync(
            notification.JournalEntryId,
            notification.EntryDate,
            notification.Description,
            "DRAFT",
            false,
            notification.OccurredAt,
            notification.OccurredAt,
            null,
            null
        );

        // 仕訳明細も保存
        foreach (var lineItem in notification.LineItems)
        {
            await _readModelRepository.InsertJournalEntryLineAsync(
                notification.JournalEntryId,
                lineItem.AccountCode,
                lineItem.DebitCredit.ToString(),
                lineItem.Amount
            );
        }
    }

    public async Task Handle(JournalEntryApprovedEvent notification, CancellationToken cancellationToken)
    {
        _logger.LogInformation("Projecting JournalEntryApprovedEvent: {JournalEntryId}",
            notification.JournalEntryId);

        await _readModelRepository.UpdateJournalEntryStatusAsync(
            notification.JournalEntryId,
            "APPROVED",
            notification.OccurredAt,
            notification.ApprovedBy,
            notification.ApprovalComment
        );
    }

    public async Task Handle(JournalEntryDeletedEvent notification, CancellationToken cancellationToken)
    {
        _logger.LogInformation("Projecting JournalEntryDeletedEvent: {JournalEntryId}",
            notification.JournalEntryId);

        await _readModelRepository.MarkAsDeletedAsync(
            notification.JournalEntryId,
            notification.OccurredAt
        );
    }
}
