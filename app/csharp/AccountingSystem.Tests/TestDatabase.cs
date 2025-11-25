using FluentMigrator.Runner;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Npgsql;
using Testcontainers.PostgreSql;

namespace AccountingSystem.Tests
{
    /// <summary>
    /// テスト用データベース管理クラス
    ///
    /// TestContainersでPostgreSQLコンテナを起動し、
    /// FluentMigratorマイグレーションを実行してテーブルを作成します。
    /// </summary>
    public class TestDatabase
    {
        private readonly PostgreSqlContainer _container;
        private NpgsqlConnection? _connection;

        public TestDatabase(PostgreSqlContainer container)
        {
            _container = container;
        }

        /// <summary>
        /// データベース接続を初期化し、FluentMigratorマイグレーションを実行
        /// </summary>
        public async Task StartAsync()
        {
            _connection = new NpgsqlConnection(_container.GetConnectionString());
            await _connection.OpenAsync();
            await RunMigrationsAsync();
        }

        /// <summary>
        /// データベース接続をクローズ
        /// </summary>
        public async Task StopAsync()
        {
            if (_connection != null)
            {
                await _connection.DisposeAsync();
            }
        }

        /// <summary>
        /// すべてのテーブルのデータをクリア
        /// </summary>
        public async Task CleanupAsync()
        {
            await using var cmd = new NpgsqlCommand(@"TRUNCATE TABLE ""勘定科目マスタ"" CASCADE", _connection);
            await cmd.ExecuteNonQueryAsync();
        }

        /// <summary>
        /// データベース接続を取得
        /// </summary>
        public NpgsqlConnection GetConnection()
        {
            return _connection!;
        }

        /// <summary>
        /// FluentMigratorマイグレーション実行
        /// </summary>
        private Task RunMigrationsAsync()
        {
            var serviceProvider = new ServiceCollection()
                .AddFluentMigratorCore()
                .ConfigureRunner(rb => rb
                    .AddPostgres()
                    .WithGlobalConnectionString(_container.GetConnectionString())
                    .ScanIn(typeof(AccountingSystem.Infrastructure.MigrationRunner).Assembly)
                    .For.Migrations())
                .AddLogging(lb => lb.AddFluentMigratorConsole())
                .BuildServiceProvider(false);

            using (var scope = serviceProvider.CreateScope())
            {
                var runner = scope.ServiceProvider.GetRequiredService<IMigrationRunner>();
                runner.MigrateUp();
            }

            return Task.CompletedTask;
        }
    }
}
