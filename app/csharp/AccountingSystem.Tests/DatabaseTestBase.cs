using FluentMigrator.Runner;
using Microsoft.Extensions.DependencyInjection;
using Testcontainers.PostgreSql;
using Xunit;

namespace AccountingSystem.Tests
{
    /// <summary>
    /// データベーステスト用の基底クラス
    /// Testcontainersを使用してPostgreSQLコンテナを起動し、テストを実行する
    ///
    /// このクラスを継承することで、以下の機能が利用できます：
    /// - PostgreSQLコンテナの自動起動・停止
    /// - FluentMigratorマイグレーションの自動実行
    /// - データベース接続文字列の提供
    /// </summary>
    public abstract class DatabaseTestBase : IAsyncLifetime
    {
        private PostgreSqlContainer? _postgres;
        protected string ConnectionString { get; private set; } = string.Empty;

        /// <summary>
        /// テスト開始前にPostgreSQLコンテナを起動し、マイグレーションを実行
        /// </summary>
        public async Task InitializeAsync()
        {
            // PostgreSQLコンテナの設定と起動
            _postgres = new PostgreSqlBuilder()
                .WithImage("postgres:16-alpine")
                .WithDatabase("accounting_system_test")
                .WithUsername("test")
                .WithPassword("test")
                .Build();

            await _postgres.StartAsync();
            ConnectionString = _postgres.GetConnectionString();

            // FluentMigratorでマイグレーションを実行
            var serviceProvider = CreateServices();
            using (var scope = serviceProvider.CreateScope())
            {
                UpdateDatabase(scope.ServiceProvider);
            }
        }

        /// <summary>
        /// テスト終了後にPostgreSQLコンテナを停止・破棄
        /// </summary>
        public async Task DisposeAsync()
        {
            if (_postgres != null)
            {
                await _postgres.DisposeAsync();
            }
        }

        private IServiceProvider CreateServices()
        {
            return new ServiceCollection()
                .AddFluentMigratorCore()
                .ConfigureRunner(rb => rb
                    .AddPostgres()
                    .WithGlobalConnectionString(ConnectionString)
                    .ScanIn(typeof(AccountingSystem.Infrastructure.MigrationRunner).Assembly)
                    .For.Migrations())
                .AddLogging(lb => lb.AddFluentMigratorConsole())
                .BuildServiceProvider(false);
        }

        private static void UpdateDatabase(IServiceProvider serviceProvider)
        {
            var runner = serviceProvider.GetRequiredService<IMigrationRunner>();
            runner.MigrateUp();
        }
    }
}
