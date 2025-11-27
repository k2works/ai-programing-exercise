using RabbitMQ.Client;
using Testcontainers.RabbitMq;

namespace AccountingSystem.Tests;

/// <summary>
/// RabbitMQ テスト用の基底クラス
/// Testcontainers を使用して RabbitMQ コンテナを起動し、テストを実行する
///
/// このクラスを継承することで、以下の機能が利用できます：
/// - RabbitMQ コンテナの自動起動・停止
/// - コネクションファクトリの提供
/// </summary>
public abstract class RabbitMqTestBase : IAsyncLifetime
{
    private RabbitMqContainer? _rabbitMqContainer;
    protected IConnectionFactory ConnectionFactory { get; private set; } = null!;
    protected string RabbitMqHostname { get; private set; } = string.Empty;
    protected int RabbitMqPort { get; private set; }

    /// <summary>
    /// テスト開始前に RabbitMQ コンテナを起動
    /// </summary>
    public async Task InitializeAsync()
    {
        _rabbitMqContainer = new RabbitMqBuilder()
            .WithUsername("admin")
            .WithPassword("admin123")
            .Build();

        await _rabbitMqContainer.StartAsync();

        RabbitMqHostname = _rabbitMqContainer.Hostname;
        RabbitMqPort = _rabbitMqContainer.GetMappedPublicPort(5672);

        ConnectionFactory = new ConnectionFactory
        {
            HostName = RabbitMqHostname,
            Port = RabbitMqPort,
            UserName = "admin",
            Password = "admin123"
        };

        // サブクラスの初期化処理
        await OnInitializedAsync();
    }

    /// <summary>
    /// サブクラスでオーバーライドして初期化処理を追加
    /// </summary>
    protected virtual Task OnInitializedAsync() => Task.CompletedTask;

    /// <summary>
    /// テスト終了後に RabbitMQ コンテナを停止・破棄
    /// </summary>
    public async Task DisposeAsync()
    {
        if (_rabbitMqContainer != null)
        {
            await _rabbitMqContainer.DisposeAsync();
        }
    }
}
