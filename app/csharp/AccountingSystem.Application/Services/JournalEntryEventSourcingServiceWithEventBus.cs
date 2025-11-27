namespace AccountingSystem.Application.Services;

using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Aggregates;
using AccountingSystem.Domain.Events;
using Microsoft.Extensions.Logging;

/// <summary>
/// イベントバス機能付き仕訳イベントソーシングサービス
/// 仕訳操作時に RabbitMQ へイベントをパブリッシュするデコレーター実装
/// </summary>
public class JournalEntryEventSourcingServiceWithEventBus : IJournalEntryEventSourcingService
{
    private readonly IJournalEntryEventSourcingService _innerService;
    private readonly IEventPublisher _eventPublisher;
    private readonly ILogger<JournalEntryEventSourcingServiceWithEventBus> _logger;

    // ルーティングキープレフィックス
    private const string RoutingKeyPrefix = "financial.journalentry";

    public JournalEntryEventSourcingServiceWithEventBus(
        IJournalEntryEventSourcingService innerService,
        IEventPublisher eventPublisher,
        ILogger<JournalEntryEventSourcingServiceWithEventBus> logger)
    {
        _innerService = innerService;
        _eventPublisher = eventPublisher;
        _logger = logger;
    }

    /// <inheritdoc />
    public async Task<string> CreateJournalEntryAsync(
        DateOnly entryDate,
        string description,
        List<LineItemDto> lineItems,
        string userId)
    {
        // 1. 内部サービスで仕訳作成
        var journalEntryId = await _innerService.CreateJournalEntryAsync(
            entryDate, description, lineItems, userId);

        // 2. ドメインイベントを作成してパブリッシュ
        var domainEvent = new JournalEntryCreatedEvent
        {
            JournalEntryId = journalEntryId,
            EntryDate = entryDate,
            Description = description,
            LineItems = lineItems.Select(dto => new JournalEntryCreatedEvent.JournalEntryLineItem
            {
                AccountCode = dto.AccountCode,
                DebitCredit = Enum.Parse<JournalEntryCreatedEvent.DebitCreditType>(dto.DebitCredit),
                Amount = dto.Amount
            }).ToList(),
            UserId = userId,
            OccurredAt = DateTime.UtcNow
        };

        await PublishEventSafelyAsync(domainEvent, $"{RoutingKeyPrefix}.created");

        _logger.LogInformation(
            "仕訳作成イベントをパブリッシュ: JournalEntryId={JournalEntryId}",
            journalEntryId);

        return journalEntryId;
    }

    /// <inheritdoc />
    public async Task ApproveJournalEntryAsync(string journalEntryId, string approvedBy, string comment)
    {
        // 1. 内部サービスで仕訳承認
        await _innerService.ApproveJournalEntryAsync(journalEntryId, approvedBy, comment);

        // 2. ドメインイベントを作成してパブリッシュ
        var domainEvent = new JournalEntryApprovedEvent
        {
            JournalEntryId = journalEntryId,
            ApprovedBy = approvedBy,
            ApprovalComment = comment,
            UserId = approvedBy,
            OccurredAt = DateTime.UtcNow
        };

        await PublishEventSafelyAsync(domainEvent, $"{RoutingKeyPrefix}.approved");

        _logger.LogInformation(
            "仕訳承認イベントをパブリッシュ: JournalEntryId={JournalEntryId}",
            journalEntryId);
    }

    /// <inheritdoc />
    public async Task DeleteJournalEntryAsync(string journalEntryId, string reason, string userId)
    {
        // 1. 内部サービスで仕訳削除
        await _innerService.DeleteJournalEntryAsync(journalEntryId, reason, userId);

        // 2. ドメインイベントを作成してパブリッシュ
        var domainEvent = new JournalEntryDeletedEvent
        {
            JournalEntryId = journalEntryId,
            Reason = reason,
            UserId = userId,
            OccurredAt = DateTime.UtcNow
        };

        await PublishEventSafelyAsync(domainEvent, $"{RoutingKeyPrefix}.deleted");

        _logger.LogInformation(
            "仕訳削除イベントをパブリッシュ: JournalEntryId={JournalEntryId}",
            journalEntryId);
    }

    /// <inheritdoc />
    public Task<JournalEntryAggregate> GetJournalEntryAsync(string journalEntryId)
    {
        return _innerService.GetJournalEntryAsync(journalEntryId);
    }

    /// <inheritdoc />
    public Task<JournalEntryAggregate> GetJournalEntryAtAsync(string journalEntryId, DateTime pointInTime)
    {
        return _innerService.GetJournalEntryAtAsync(journalEntryId, pointInTime);
    }

    /// <summary>
    /// イベントを安全にパブリッシュ（失敗してもメイン処理は継続）
    /// </summary>
    private async Task PublishEventSafelyAsync<TEvent>(TEvent @event, string routingKey)
    {
        try
        {
            await _eventPublisher.PublishAsync(@event, routingKey);
        }
        catch (Exception ex)
        {
            // イベントパブリッシュの失敗はログに記録するが、メイン処理は継続
            _logger.LogError(ex,
                "イベントパブリッシュ失敗（メイン処理は継続）: RoutingKey={RoutingKey}",
                routingKey);
        }
    }
}
