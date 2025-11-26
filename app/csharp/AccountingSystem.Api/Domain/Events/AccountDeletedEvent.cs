namespace AccountingSystem.Domain.Events;

/// <summary>
/// 勘定科目削除イベント
/// </summary>
public record AccountDeletedEvent : DomainEvent
{
    public required string AccountCode { get; init; }
    public required Dictionary<string, object> DeletedData { get; init; }
    public required string UserId { get; init; }
    public required string UserName { get; init; }
    public string? Reason { get; init; }
    public string? IpAddress { get; init; }

    public override string GetEventType() => "AccountDeleted";
}
