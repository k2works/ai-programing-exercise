using FinancialAccounting.Domain.Events;

namespace FinancialAccounting.Application.Ports.Out;

/// <summary>
/// イベント発行（出力ポート）
/// </summary>
public interface IEventPublisher
{
    Task PublishAsync(JournalCreatedEvent @event);
}
