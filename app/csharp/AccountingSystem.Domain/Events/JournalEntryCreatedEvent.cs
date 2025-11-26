namespace AccountingSystem.Domain.Events;

/// <summary>
/// 仕訳作成イベント（イベントソーシング用）
/// </summary>
public record JournalEntryCreatedEvent : IEventSourcedDomainEvent
{
    public required string JournalEntryId { get; init; }
    public required DateOnly EntryDate { get; init; }
    public required string Description { get; init; }
    public required List<JournalEntryLineItem> LineItems { get; init; }
    public required string UserId { get; init; }
    public required DateTime OccurredAt { get; init; }

    public string AggregateId => JournalEntryId;
    public string EventType => "JournalEntryCreatedEvent";
    public int EventVersion => 1;

    /// <summary>
    /// 仕訳明細
    /// </summary>
    public record JournalEntryLineItem
    {
        public required string AccountCode { get; init; }
        public required DebitCreditType DebitCredit { get; init; }
        public required decimal Amount { get; init; }
    }

    /// <summary>
    /// 借方・貸方区分
    /// </summary>
    public enum DebitCreditType
    {
        DEBIT,
        CREDIT
    }
}
