using FluentMigrator.Runner;
using Microsoft.Extensions.DependencyInjection;
using Npgsql;
using Testcontainers.PostgreSql;

namespace ProductionManagement.Tests.TestSetup;

/// <summary>
/// データベーステスト用の基底クラス
/// Testcontainers を使用して PostgreSQL コンテナを起動し、テストを実行する
///
/// このクラスを継承することで、以下の機能が利用できます：
/// - PostgreSQL コンテナの自動起動・停止
/// - FluentMigrator マイグレーションの自動実行
/// - データベース接続文字列の提供
/// </summary>
public abstract class DatabaseTestBase : IAsyncLifetime
{
    private PostgreSqlContainer? _postgres;

    protected string ConnectionString { get; private set; } = string.Empty;

    /// <summary>
    /// テスト開始前に PostgreSQL コンテナを起動し、マイグレーションを実行
    /// </summary>
    public async Task InitializeAsync()
    {
        // PostgreSQL コンテナの設定と起動
        _postgres = new PostgreSqlBuilder()
            .WithImage("postgres:16-alpine")
            .WithDatabase("production_management_test")
            .WithUsername("test")
            .WithPassword("test")
            .Build();

        await _postgres.StartAsync();
        ConnectionString = _postgres.GetConnectionString();

        // FluentMigrator でマイグレーションを実行
        var serviceProvider = CreateServices();
        using (var scope = serviceProvider.CreateScope())
        {
            UpdateDatabase(scope.ServiceProvider);
        }
    }

    /// <summary>
    /// テスト終了後に PostgreSQL コンテナを停止・破棄
    /// </summary>
    public async Task DisposeAsync()
    {
        if (_postgres != null)
        {
            await _postgres.DisposeAsync();
        }
    }

    /// <summary>
    /// データベース接続を作成する
    /// </summary>
    public NpgsqlConnection CreateConnection()
    {
        return new NpgsqlConnection(ConnectionString);
    }

    private static ServiceProvider CreateServices(string connectionString)
    {
        return new ServiceCollection()
            .AddFluentMigratorCore()
            .ConfigureRunner(rb => rb
                .AddPostgres()
                .WithGlobalConnectionString(connectionString)
                .ScanIn(typeof(Infrastructure.Database.MigrationRunner).Assembly)
                .For.Migrations())
            .AddLogging(lb => lb.AddFluentMigratorConsole())
            .BuildServiceProvider(false);
    }

    private ServiceProvider CreateServices()
    {
        return CreateServices(ConnectionString);
    }

    private static void UpdateDatabase(IServiceProvider serviceProvider)
    {
        var runner = serviceProvider.GetRequiredService<IMigrationRunner>();
        runner.MigrateUp();
    }
}
