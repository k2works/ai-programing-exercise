using AccountingSystem.Application.Ports.In;
using AccountingSystem.Domain.Events;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using RabbitMQ.Client;

namespace AccountingSystem.Infrastructure.EventBus;

/// <summary>
/// 仕訳作成イベント購読バックグラウンドサービス
/// </summary>
public class JournalEntryCreatedEventConsumerBackgroundService : BackgroundService
{
    private readonly IServiceProvider _serviceProvider;
    private readonly IConnectionFactory _connectionFactory;
    private readonly ILogger<JournalEntryCreatedEventConsumerBackgroundService> _logger;
    private IEventConsumer? _consumer;

    private const string ExchangeName = "financial-events";
    private const string QueueName = "journal-created-queue";
    private const string RoutingKey = "financial.journalentry.created";

    public JournalEntryCreatedEventConsumerBackgroundService(
        IServiceProvider serviceProvider,
        IConnectionFactory connectionFactory,
        ILogger<JournalEntryCreatedEventConsumerBackgroundService> logger)
    {
        _serviceProvider = serviceProvider;
        _connectionFactory = connectionFactory;
        _logger = logger;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        _logger.LogInformation("仕訳作成イベントコンシューマー開始");

        using var scope = _serviceProvider.CreateScope();
        var eventHandler = scope.ServiceProvider.GetRequiredService<IEventBusHandler<JournalEntryCreatedEvent>>();

        _consumer = new RabbitMQEventConsumer<JournalEntryCreatedEvent>(
            _connectionFactory,
            ExchangeName,
            QueueName,
            RoutingKey,
            eventHandler,
            _serviceProvider.GetRequiredService<ILogger<RabbitMQEventConsumer<JournalEntryCreatedEvent>>>()
        );

        await _consumer.StartAsync(stoppingToken);

        // サービス停止まで待機
        while (!stoppingToken.IsCancellationRequested)
        {
            await Task.Delay(1000, stoppingToken);
        }
    }

    public override async Task StopAsync(CancellationToken cancellationToken)
    {
        _logger.LogInformation("仕訳作成イベントコンシューマー停止");
        if (_consumer != null)
        {
            await _consumer.StopAsync(cancellationToken);
        }
        await base.StopAsync(cancellationToken);
    }
}

/// <summary>
/// 仕訳承認イベント購読バックグラウンドサービス
/// </summary>
public class JournalEntryApprovedEventConsumerBackgroundService : BackgroundService
{
    private readonly IServiceProvider _serviceProvider;
    private readonly IConnectionFactory _connectionFactory;
    private readonly ILogger<JournalEntryApprovedEventConsumerBackgroundService> _logger;
    private IEventConsumer? _consumer;

    private const string ExchangeName = "financial-events";
    private const string QueueName = "journal-approved-queue";
    private const string RoutingKey = "financial.journalentry.approved";

    public JournalEntryApprovedEventConsumerBackgroundService(
        IServiceProvider serviceProvider,
        IConnectionFactory connectionFactory,
        ILogger<JournalEntryApprovedEventConsumerBackgroundService> logger)
    {
        _serviceProvider = serviceProvider;
        _connectionFactory = connectionFactory;
        _logger = logger;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        _logger.LogInformation("仕訳承認イベントコンシューマー開始");

        using var scope = _serviceProvider.CreateScope();
        var eventHandler = scope.ServiceProvider.GetRequiredService<IEventBusHandler<JournalEntryApprovedEvent>>();

        _consumer = new RabbitMQEventConsumer<JournalEntryApprovedEvent>(
            _connectionFactory,
            ExchangeName,
            QueueName,
            RoutingKey,
            eventHandler,
            _serviceProvider.GetRequiredService<ILogger<RabbitMQEventConsumer<JournalEntryApprovedEvent>>>()
        );

        await _consumer.StartAsync(stoppingToken);

        while (!stoppingToken.IsCancellationRequested)
        {
            await Task.Delay(1000, stoppingToken);
        }
    }

    public override async Task StopAsync(CancellationToken cancellationToken)
    {
        _logger.LogInformation("仕訳承認イベントコンシューマー停止");
        if (_consumer != null)
        {
            await _consumer.StopAsync(cancellationToken);
        }
        await base.StopAsync(cancellationToken);
    }
}

/// <summary>
/// 仕訳削除イベント購読バックグラウンドサービス
/// </summary>
public class JournalEntryDeletedEventConsumerBackgroundService : BackgroundService
{
    private readonly IServiceProvider _serviceProvider;
    private readonly IConnectionFactory _connectionFactory;
    private readonly ILogger<JournalEntryDeletedEventConsumerBackgroundService> _logger;
    private IEventConsumer? _consumer;

    private const string ExchangeName = "financial-events";
    private const string QueueName = "journal-deleted-queue";
    private const string RoutingKey = "financial.journalentry.deleted";

    public JournalEntryDeletedEventConsumerBackgroundService(
        IServiceProvider serviceProvider,
        IConnectionFactory connectionFactory,
        ILogger<JournalEntryDeletedEventConsumerBackgroundService> logger)
    {
        _serviceProvider = serviceProvider;
        _connectionFactory = connectionFactory;
        _logger = logger;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        _logger.LogInformation("仕訳削除イベントコンシューマー開始");

        using var scope = _serviceProvider.CreateScope();
        var eventHandler = scope.ServiceProvider.GetRequiredService<IEventBusHandler<JournalEntryDeletedEvent>>();

        _consumer = new RabbitMQEventConsumer<JournalEntryDeletedEvent>(
            _connectionFactory,
            ExchangeName,
            QueueName,
            RoutingKey,
            eventHandler,
            _serviceProvider.GetRequiredService<ILogger<RabbitMQEventConsumer<JournalEntryDeletedEvent>>>()
        );

        await _consumer.StartAsync(stoppingToken);

        while (!stoppingToken.IsCancellationRequested)
        {
            await Task.Delay(1000, stoppingToken);
        }
    }

    public override async Task StopAsync(CancellationToken cancellationToken)
    {
        _logger.LogInformation("仕訳削除イベントコンシューマー停止");
        if (_consumer != null)
        {
            await _consumer.StopAsync(cancellationToken);
        }
        await base.StopAsync(cancellationToken);
    }
}
