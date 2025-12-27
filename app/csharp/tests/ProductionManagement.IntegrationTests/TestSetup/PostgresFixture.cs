using FluentMigrator.Runner;
using Microsoft.Extensions.DependencyInjection;
using Npgsql;
using Testcontainers.PostgreSql;

namespace ProductionManagement.IntegrationTests.TestSetup;

/// <summary>
/// PostgreSQL Fixture - 統合テスト用の共有コンテナ
/// </summary>
public class PostgresFixture : IAsyncLifetime
{
    private readonly PostgreSqlContainer _postgres = new PostgreSqlBuilder()
        .WithImage("postgres:16-alpine")
        .WithDatabase("testdb")
        .WithUsername("testuser")
        .WithPassword("testpass")
        .Build();

    public string ConnectionString => _postgres.GetConnectionString();

    public async Task InitializeAsync()
    {
        await _postgres.StartAsync();

        // FluentMigrator でマイグレーション実行
        var serviceProvider = new ServiceCollection()
            .AddFluentMigratorCore()
            .ConfigureRunner(rb => rb
                .AddPostgres()
                .WithGlobalConnectionString(ConnectionString)
                .ScanIn(typeof(Infrastructure.Database.MigrationRunner).Assembly)
                .For.Migrations())
            .AddLogging(lb => lb.AddFluentMigratorConsole())
            .BuildServiceProvider(false);

        using var scope = serviceProvider.CreateScope();
        var runner = scope.ServiceProvider.GetRequiredService<IMigrationRunner>();
        runner.MigrateUp();
    }

    public async Task DisposeAsync()
    {
        await _postgres.DisposeAsync();
    }

    public NpgsqlConnection CreateConnection()
    {
        return new NpgsqlConnection(ConnectionString);
    }

    /// <summary>
    /// テストデータをクリアする
    /// </summary>
    public async Task CleanDatabaseAsync()
    {
        await using var conn = CreateConnection();
        await conn.OpenAsync();

        // 外部キー制約を考慮してテーブルを削除順序で TRUNCATE
        // CASCADE を使用して依存するテーブルも自動でクリア
        var tables = new[]
        {
            // トランザクションデータ（子テーブルから順番に）
            "\"引当情報\"",
            "\"所要情報\"",
            "\"作業指示明細データ\"",
            "\"作業指示データ\"",
            "\"検収データ\"",
            "\"受入検査データ\"",
            "\"入荷受入データ\"",
            "\"発注明細データ\"",
            "\"諸口品目情報\"",
            "\"発注データ\"",
            "\"オーダ情報\"",
            "\"基準生産計画\"",
            "\"在庫情報\"",
            // マスタデータ
            "\"工程表\"",
            "\"工程マスタ\"",
            "\"単価マスタ\"",
            "\"部品構成表\"",
            "\"品目マスタ\"",
            "\"取引先マスタ\"",
            "\"場所マスタ\"",
            "\"欠点マスタ\""
        };

        foreach (var table in tables)
        {
            await using var cmd = conn.CreateCommand();
            cmd.CommandText = $"TRUNCATE TABLE {table} CASCADE";
            await cmd.ExecuteNonQueryAsync();
        }
    }
}

/// <summary>
/// xUnit Collection Definition
/// </summary>
[CollectionDefinition("Postgres")]
public class PostgresCollection : ICollectionFixture<PostgresFixture>
{
}
