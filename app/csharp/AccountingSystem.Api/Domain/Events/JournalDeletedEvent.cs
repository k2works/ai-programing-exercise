namespace AccountingSystem.Domain.Events;

/// <summary>
/// 仕訳削除イベント
/// </summary>
public record JournalDeletedEvent : DomainEvent
{
    public required string JournalNo { get; init; }
    public required Dictionary<string, object> DeletedData { get; init; }
    public required string UserId { get; init; }
    public required string UserName { get; init; }
    public string? Reason { get; init; }
    public string? IpAddress { get; init; }

    public override string GetEventType() => "JournalDeleted";
}
