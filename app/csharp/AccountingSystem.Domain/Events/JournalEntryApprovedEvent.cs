namespace AccountingSystem.Domain.Events;

/// <summary>
/// 仕訳承認イベント（イベントソーシング用）
/// </summary>
public record JournalEntryApprovedEvent : IEventSourcedDomainEvent
{
    public required string JournalEntryId { get; init; }
    public required string ApprovedBy { get; init; }
    public required string ApprovalComment { get; init; }
    public required DateTime OccurredAt { get; init; }
    public required string UserId { get; init; }

    public string AggregateId => JournalEntryId;
    public string EventType => "JournalEntryApprovedEvent";
    public int EventVersion => 1;
}
