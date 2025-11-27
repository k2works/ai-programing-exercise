using System.Text;
using System.Text.Json;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;
using Microsoft.Extensions.Logging;
using AccountingSystem.Application.Ports.In;

namespace AccountingSystem.Infrastructure.EventBus;

/// <summary>
/// RabbitMQ イベントコンシューマー実装
/// 指定したキューからイベントを購読し、ハンドラーに委譲
/// </summary>
/// <typeparam name="TEvent">購読するイベントの型</typeparam>
public class RabbitMQEventConsumer<TEvent> : IEventConsumer, IAsyncDisposable
{
    private readonly IConnectionFactory _connectionFactory;
    private readonly string _exchangeName;
    private readonly string _queueName;
    private readonly string _routingKey;
    private readonly IEventBusHandler<TEvent> _eventHandler;
    private readonly ILogger<RabbitMQEventConsumer<TEvent>> _logger;
    private IConnection? _connection;
    private IChannel? _channel;
    private readonly JsonSerializerOptions _jsonOptions = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        PropertyNameCaseInsensitive = true
    };

    public RabbitMQEventConsumer(
        IConnectionFactory connectionFactory,
        string exchangeName,
        string queueName,
        string routingKey,
        IEventBusHandler<TEvent> eventHandler,
        ILogger<RabbitMQEventConsumer<TEvent>> logger)
    {
        _connectionFactory = connectionFactory;
        _exchangeName = exchangeName;
        _queueName = queueName;
        _routingKey = routingKey;
        _eventHandler = eventHandler;
        _logger = logger;
    }

    /// <inheritdoc />
    public async Task StartAsync(CancellationToken cancellationToken = default)
    {
        try
        {
            // 接続を確立
            _connection = await _connectionFactory.CreateConnectionAsync(cancellationToken);
            _channel = await _connection.CreateChannelAsync(cancellationToken: cancellationToken);

            // Topic Exchange を宣言
            await _channel.ExchangeDeclareAsync(
                exchange: _exchangeName,
                type: ExchangeType.Topic,
                durable: true,
                autoDelete: false,
                cancellationToken: cancellationToken
            );

            // Dead Letter Queue 設定付きでキューを宣言
            var arguments = new Dictionary<string, object?>
            {
                { "x-dead-letter-exchange", $"{_exchangeName}-dlq" },
                { "x-dead-letter-routing-key", $"dlq.{_routingKey}" }
            };

            await _channel.QueueDeclareAsync(
                queue: _queueName,
                durable: true,
                exclusive: false,
                autoDelete: false,
                arguments: arguments,
                cancellationToken: cancellationToken
            );

            // キューを Exchange にバインド
            await _channel.QueueBindAsync(
                queue: _queueName,
                exchange: _exchangeName,
                routingKey: _routingKey,
                cancellationToken: cancellationToken
            );

            // メッセージ受信設定
            var consumer = new AsyncEventingBasicConsumer(_channel);

            consumer.ReceivedAsync += async (sender, ea) =>
            {
                try
                {
                    // JSON をデシリアライズ
                    var body = ea.Body.ToArray();
                    var json = Encoding.UTF8.GetString(body);
                    var @event = JsonSerializer.Deserialize<TEvent>(json, _jsonOptions);

                    if (@event != null)
                    {
                        // イベントハンドラーを実行
                        await _eventHandler.HandleAsync(@event);

                        // ACK を送信
                        await _channel.BasicAckAsync(ea.DeliveryTag, multiple: false);

                        _logger.LogInformation(
                            "イベント処理完了: Queue={Queue}, RoutingKey={RoutingKey}",
                            _queueName, _routingKey);
                    }
                    else
                    {
                        _logger.LogWarning(
                            "イベントのデシリアライズ結果が null: Queue={Queue}",
                            _queueName);
                        await _channel.BasicAckAsync(ea.DeliveryTag, multiple: false);
                    }
                }
                catch (Exception ex)
                {
                    _logger.LogError(ex,
                        "イベント処理失敗: Queue={Queue}, RoutingKey={RoutingKey}",
                        _queueName, _routingKey);

                    // NACK を送信（リキュー）
                    await _channel.BasicNackAsync(ea.DeliveryTag, multiple: false, requeue: true);
                }
            };

            // 購読開始
            await _channel.BasicConsumeAsync(
                queue: _queueName,
                autoAck: false,
                consumer: consumer,
                cancellationToken: cancellationToken
            );

            _logger.LogInformation(
                "RabbitMQ コンシューマー開始: Queue={Queue}, RoutingKey={RoutingKey}",
                _queueName, _routingKey);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "コンシューマー開始失敗");
            throw;
        }
    }

    /// <inheritdoc />
    public async Task StopAsync(CancellationToken cancellationToken = default)
    {
        if (_channel?.IsOpen == true)
        {
            await _channel.CloseAsync(cancellationToken);
        }

        if (_connection?.IsOpen == true)
        {
            await _connection.CloseAsync(cancellationToken);
        }

        _logger.LogInformation("RabbitMQ コンシューマー停止: Queue={Queue}", _queueName);
    }

    /// <inheritdoc />
    public async ValueTask DisposeAsync()
    {
        await StopAsync();
        _channel?.Dispose();
        _connection?.Dispose();
        GC.SuppressFinalize(this);
    }
}
