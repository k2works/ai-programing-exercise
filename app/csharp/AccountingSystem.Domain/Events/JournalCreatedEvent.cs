namespace AccountingSystem.Domain.Events;

/// <summary>
/// 仕訳作成イベント
/// </summary>
public record JournalCreatedEvent : DomainEvent
{
    public required string JournalNo { get; init; }
    public required DateOnly JournalDate { get; init; }
    public required Dictionary<string, object> JournalData { get; init; }
    public required string UserId { get; init; }
    public required string UserName { get; init; }
    public string? IpAddress { get; init; }

    public override string GetEventType() => "JournalCreated";
}
