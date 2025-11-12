using DotNet.Testcontainers.Containers;
using FluentMigrator.Runner;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Testcontainers.MySql;
using Testcontainers.PostgreSql;
using Xunit;

namespace SalesManagement.Tests
{
    /// <summary>
    /// データベーステスト用の基底クラス
    /// Testcontainersを使用してPostgreSQLまたはMySQLコンテナを起動し、テストを実行する
    ///
    /// このクラスを継承することで、以下の機能が利用できます：
    /// - PostgreSQL/MySQLコンテナの自動起動・停止
    /// - FluentMigratorマイグレーションの自動実行
    /// - データベース接続文字列の提供
    /// </summary>
    public abstract class DatabaseTestBase : IAsyncLifetime
    {
        private IContainer? _container;
        protected string ConnectionString { get; private set; } = string.Empty;
        protected string DatabaseType { get; private set; } = "PostgreSQL";

        /// <summary>
        /// テスト開始前にデータベースコンテナを起動し、マイグレーションを実行
        /// </summary>
        public async Task InitializeAsync()
        {
            // 設定ファイルから DatabaseType を読み取る
            var configuration = new ConfigurationBuilder()
                .SetBasePath(Directory.GetCurrentDirectory())
                .AddJsonFile("appsettings.Test.json", optional: false)
                .Build();

            DatabaseType = configuration["DatabaseType"] ?? "PostgreSQL";

            // DatabaseType に応じてコンテナを起動
            if (DatabaseType == "MySQL")
            {
                var mysqlContainer = new MySqlBuilder()
                    .WithImage("mysql:8.0")
                    .WithDatabase("sales_management_test")
                    .WithUsername("test")
                    .WithPassword("test")
                    .Build();

                await mysqlContainer.StartAsync();
                _container = mysqlContainer;
                ConnectionString = mysqlContainer.GetConnectionString();
            }
            else
            {
                var postgresContainer = new PostgreSqlBuilder()
                    .WithImage("postgres:16-alpine")
                    .WithDatabase("sales_management_test")
                    .WithUsername("test")
                    .WithPassword("test")
                    .Build();

                await postgresContainer.StartAsync();
                _container = postgresContainer;
                ConnectionString = postgresContainer.GetConnectionString();
            }

            // FluentMigratorでマイグレーションを実行
            var serviceProvider = CreateServices();
            using (var scope = serviceProvider.CreateScope())
            {
                UpdateDatabase(scope.ServiceProvider);
            }
        }

        /// <summary>
        /// テスト終了後にデータベースコンテナを停止・破棄
        /// </summary>
        public async Task DisposeAsync()
        {
            if (_container != null)
            {
                await _container.DisposeAsync();
            }
        }

        private IServiceProvider CreateServices()
        {
            var services = new ServiceCollection()
                .AddFluentMigratorCore()
                .ConfigureRunner(rb =>
                {
                    if (DatabaseType == "MySQL")
                    {
                        rb.AddMySql8();
                    }
                    else
                    {
                        rb.AddPostgres();
                    }

                    rb.WithGlobalConnectionString(ConnectionString)
                        .ScanIn(typeof(SalesManagement.Infrastructure.MigrationRunner).Assembly)
                        .For.Migrations();
                })
                .AddLogging(lb => lb.AddFluentMigratorConsole());

            return services.BuildServiceProvider(false);
        }

        private static void UpdateDatabase(IServiceProvider serviceProvider)
        {
            var runner = serviceProvider.GetRequiredService<IMigrationRunner>();
            runner.MigrateUp();
        }
    }
}
