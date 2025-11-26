namespace AccountingSystem.Domain.Events;

/// <summary>
/// 勘定科目更新イベント
/// </summary>
public record AccountUpdatedEvent : DomainEvent
{
    public required string AccountCode { get; init; }
    public required Dictionary<string, object> OldValues { get; init; }
    public required Dictionary<string, object> NewValues { get; init; }
    public required string UserId { get; init; }
    public required string UserName { get; init; }
    public string? IpAddress { get; init; }

    public override string GetEventType() => "AccountUpdated";
}
