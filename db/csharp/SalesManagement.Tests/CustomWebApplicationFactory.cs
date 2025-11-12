using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

namespace SalesManagement.Tests;

/// <summary>
/// Testcontainersを使用したカスタムWebApplicationFactory
/// DatabaseTestBaseを継承してデータベースコンテナの起動と接続を管理する
/// </summary>
public class CustomWebApplicationFactory : WebApplicationFactory<Program>, IAsyncLifetime
{
    private readonly TestDatabaseHelper _databaseTestBase = new();

    public string ConnectionString => _databaseTestBase.GetConnectionString();
    public string DatabaseType => _databaseTestBase.GetDatabaseType();

    protected override void ConfigureWebHost(IWebHostBuilder builder)
    {
        builder.ConfigureAppConfiguration((context, config) =>
        {
            // Testcontainersの接続文字列を使用するように設定を上書き
            config.AddInMemoryCollection(new Dictionary<string, string?>
            {
                ["ConnectionStrings:PostgreSQL"] = ConnectionString,
                ["ConnectionStrings:MySQL"] = ConnectionString,
                ["DatabaseType"] = DatabaseType
            });
        });

        builder.ConfigureServices(services =>
        {
            // 必要に応じて追加のサービス設定をここに記述
        });
    }

    public async Task InitializeAsync()
    {
        await _databaseTestBase.InitializeAsync();
        // テストデータを投入
        await _databaseTestBase.SeedTestDataAsync();
    }

    public new async Task DisposeAsync()
    {
        await _databaseTestBase.DisposeAsync();
        await base.DisposeAsync();
    }

    /// <summary>
    /// DatabaseTestBaseを実装するヘルパークラス
    /// </summary>
    private sealed class TestDatabaseHelper : DatabaseTestBase
    {
        public string GetConnectionString() => ConnectionString;
        public string GetDatabaseType() => DatabaseType;

        /// <summary>
        /// テスト用のシードデータを投入
        /// </summary>
        public async Task SeedTestDataAsync()
        {
            var seeder = new SalesManagement.Infrastructure.DataSeeder(ConnectionString, DatabaseType);
            await seeder.SeedAllAsync();
        }
    }
}
