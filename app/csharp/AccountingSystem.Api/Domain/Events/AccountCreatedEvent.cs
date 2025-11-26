namespace AccountingSystem.Domain.Events;

/// <summary>
/// 勘定科目作成イベント
/// </summary>
public record AccountCreatedEvent : DomainEvent
{
    public required string AccountCode { get; init; }
    public required Dictionary<string, object> AccountData { get; init; }
    public required string UserId { get; init; }
    public required string UserName { get; init; }
    public string? IpAddress { get; init; }

    public override string GetEventType() => "AccountCreated";
}
