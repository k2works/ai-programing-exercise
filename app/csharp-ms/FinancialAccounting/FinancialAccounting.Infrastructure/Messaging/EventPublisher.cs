using FinancialAccounting.Application.Ports.Out;
using MassTransit;
using Shared.Contracts.Events;

namespace FinancialAccounting.Infrastructure.Messaging;

/// <summary>
/// MassTransit イベント発行実装
/// </summary>
public class EventPublisher : IEventPublisher
{
    private readonly IPublishEndpoint _publishEndpoint;

    public EventPublisher(IPublishEndpoint publishEndpoint)
    {
        _publishEndpoint = publishEndpoint;
    }

    public async Task PublishAsync(JournalCreatedEvent @event)
    {
        await _publishEndpoint.Publish(@event);
    }
}
