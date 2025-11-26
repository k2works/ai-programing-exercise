using MediatR;

namespace AccountingSystem.Domain.Events;

/// <summary>
/// ドメインイベント基底クラス
/// </summary>
public abstract record DomainEvent : INotification
{
    public string EventId { get; init; } = Guid.NewGuid().ToString();
    public DateTime OccurredAt { get; init; } = DateTime.UtcNow;

    public abstract string GetEventType();
}
