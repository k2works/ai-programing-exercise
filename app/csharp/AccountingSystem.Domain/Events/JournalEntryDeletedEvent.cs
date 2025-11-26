namespace AccountingSystem.Domain.Events;

/// <summary>
/// 仕訳削除イベント（イベントソーシング用）
/// </summary>
public record JournalEntryDeletedEvent : IEventSourcedDomainEvent
{
    public required string JournalEntryId { get; init; }
    public required string Reason { get; init; }
    public required DateTime OccurredAt { get; init; }
    public required string UserId { get; init; }

    public string AggregateId => JournalEntryId;
    public string EventType => "JournalEntryDeletedEvent";
    public int EventVersion => 1;
}
