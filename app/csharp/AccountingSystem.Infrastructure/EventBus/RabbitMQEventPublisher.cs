using System.Text;
using System.Text.Json;
using RabbitMQ.Client;
using Microsoft.Extensions.Logging;
using AccountingSystem.Application.Ports.Out;

namespace AccountingSystem.Infrastructure.EventBus;

/// <summary>
/// RabbitMQ イベントパブリッシャー実装
/// Topic Exchange を使用してイベントをパブリッシュ
/// </summary>
public class RabbitMQEventPublisher : IEventPublisher, IAsyncDisposable
{
    private readonly IConnectionFactory _connectionFactory;
    private readonly string _exchangeName;
    private readonly ILogger<RabbitMQEventPublisher> _logger;
    private IConnection? _connection;
    private IChannel? _channel;
    private readonly SemaphoreSlim _semaphore = new(1, 1);
    private readonly JsonSerializerOptions _jsonOptions = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        PropertyNameCaseInsensitive = true
    };

    public RabbitMQEventPublisher(
        IConnectionFactory connectionFactory,
        string exchangeName,
        ILogger<RabbitMQEventPublisher> logger)
    {
        _connectionFactory = connectionFactory;
        _exchangeName = exchangeName;
        _logger = logger;
    }

    /// <summary>
    /// 接続を確立（Thread-safe）
    /// </summary>
    private async Task EnsureConnectedAsync()
    {
        if (_connection?.IsOpen == true && _channel?.IsOpen == true)
        {
            return;
        }

        await _semaphore.WaitAsync();
        try
        {
            if (_connection?.IsOpen == true && _channel?.IsOpen == true)
            {
                return;
            }

            // 既存の接続をクリーンアップ
            if (_channel != null)
            {
                await _channel.CloseAsync();
                _channel.Dispose();
            }
            if (_connection != null)
            {
                await _connection.CloseAsync();
                _connection.Dispose();
            }

            // 新しい接続を作成
            _connection = await _connectionFactory.CreateConnectionAsync();
            _channel = await _connection.CreateChannelAsync();

            // Topic Exchange を宣言
            await _channel.ExchangeDeclareAsync(
                exchange: _exchangeName,
                type: ExchangeType.Topic,
                durable: true,
                autoDelete: false
            );

            _logger.LogInformation("RabbitMQ 接続確立: Exchange={Exchange}", _exchangeName);
        }
        finally
        {
            _semaphore.Release();
        }
    }

    /// <inheritdoc />
    public async Task PublishAsync<TEvent>(TEvent @event, string routingKey)
    {
        try
        {
            await EnsureConnectedAsync();

            if (_channel == null)
            {
                throw new InvalidOperationException("RabbitMQ channel が利用できません");
            }

            // JSON にシリアライズ
            var json = JsonSerializer.Serialize(@event, _jsonOptions);
            var body = Encoding.UTF8.GetBytes(json);

            // 永続化プロパティを設定
            var properties = new BasicProperties
            {
                Persistent = true,
                ContentType = "application/json",
                Timestamp = new AmqpTimestamp(DateTimeOffset.UtcNow.ToUnixTimeSeconds())
            };

            // パブリッシュ
            await _channel.BasicPublishAsync(
                exchange: _exchangeName,
                routingKey: routingKey,
                mandatory: false,
                basicProperties: properties,
                body: body
            );

            _logger.LogInformation("イベントパブリッシュ完了: RoutingKey={RoutingKey}", routingKey);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "イベントパブリッシュ失敗: RoutingKey={RoutingKey}", routingKey);
            throw;
        }
    }

    /// <inheritdoc />
    public async ValueTask DisposeAsync()
    {
        if (_channel?.IsOpen == true)
        {
            await _channel.CloseAsync();
        }
        _channel?.Dispose();

        if (_connection?.IsOpen == true)
        {
            await _connection.CloseAsync();
        }
        _connection?.Dispose();

        _semaphore.Dispose();

        GC.SuppressFinalize(this);
    }
}
