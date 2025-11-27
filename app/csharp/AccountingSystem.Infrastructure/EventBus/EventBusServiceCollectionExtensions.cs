using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Application.Services;
using AccountingSystem.Domain.Events;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using RabbitMQ.Client;

namespace AccountingSystem.Infrastructure.EventBus;

/// <summary>
/// イベントバス関連のサービス登録拡張メソッド
/// </summary>
public static class EventBusServiceCollectionExtensions
{
    /// <summary>
    /// RabbitMQ イベントバスサービスを登録
    /// </summary>
    /// <param name="services">サービスコレクション</param>
    /// <param name="rabbitMqHostName">RabbitMQ ホスト名</param>
    /// <param name="rabbitMqPort">RabbitMQ ポート</param>
    /// <param name="rabbitMqUserName">RabbitMQ ユーザー名</param>
    /// <param name="rabbitMqPassword">RabbitMQ パスワード</param>
    /// <param name="exchangeName">Exchange 名</param>
    /// <returns>サービスコレクション</returns>
    public static IServiceCollection AddRabbitMQEventBus(
        this IServiceCollection services,
        string rabbitMqHostName = "localhost",
        int rabbitMqPort = 5672,
        string rabbitMqUserName = "admin",
        string rabbitMqPassword = "admin123",
        string exchangeName = "financial-events")
    {
        // RabbitMQ ConnectionFactory を登録
        services.AddSingleton<IConnectionFactory>(sp =>
        {
            return new ConnectionFactory
            {
                HostName = rabbitMqHostName,
                Port = rabbitMqPort,
                UserName = rabbitMqUserName,
                Password = rabbitMqPassword
            };
        });

        // Event Publisher を登録
        services.AddSingleton<IEventPublisher>(sp =>
        {
            var factory = sp.GetRequiredService<IConnectionFactory>();
            var logger = sp.GetRequiredService<ILogger<RabbitMQEventPublisher>>();
            return new RabbitMQEventPublisher(factory, exchangeName, logger);
        });

        // Email Service を登録（開発用ダミー）
        services.AddSingleton<IEmailService, ConsoleEmailService>();

        // Event Handlers を登録（監査ログ用）
        services.AddScoped<IEventBusHandler<JournalEntryCreatedEvent>, AuditLogJournalEntryCreatedHandler>();
        services.AddScoped<IEventBusHandler<JournalEntryApprovedEvent>, AuditLogJournalEntryApprovedHandler>();
        services.AddScoped<IEventBusHandler<JournalEntryDeletedEvent>, AuditLogJournalEntryDeletedHandler>();

        return services;
    }

    /// <summary>
    /// イベントバス連携付き仕訳サービスを登録
    /// </summary>
    /// <param name="services">サービスコレクション</param>
    /// <returns>サービスコレクション</returns>
    public static IServiceCollection AddJournalEntryEventSourcingServiceWithEventBus(
        this IServiceCollection services)
    {
        // 基本サービスを登録
        services.AddScoped<JournalEntryEventSourcingService>();

        // デコレーターパターンでイベントバス連携を追加
        services.AddScoped<JournalEntryEventSourcingServiceWithEventBus>(sp =>
        {
            var innerService = sp.GetRequiredService<JournalEntryEventSourcingService>();
            var eventPublisher = sp.GetRequiredService<IEventPublisher>();
            var logger = sp.GetRequiredService<ILogger<JournalEntryEventSourcingServiceWithEventBus>>();
            return new JournalEntryEventSourcingServiceWithEventBus(innerService, eventPublisher, logger);
        });

        // インターフェースとしても登録（既存のコントローラー向け）
        services.AddScoped<IJournalEntryEventSourcingService>(sp =>
            sp.GetRequiredService<JournalEntryEventSourcingServiceWithEventBus>());

        return services;
    }

    /// <summary>
    /// イベントコンシューマー BackgroundService を登録
    /// </summary>
    /// <param name="services">サービスコレクション</param>
    /// <returns>サービスコレクション</returns>
    public static IServiceCollection AddEventConsumerBackgroundServices(
        this IServiceCollection services)
    {
        services.AddHostedService<JournalEntryCreatedEventConsumerBackgroundService>();
        services.AddHostedService<JournalEntryApprovedEventConsumerBackgroundService>();
        services.AddHostedService<JournalEntryDeletedEventConsumerBackgroundService>();

        return services;
    }
}
