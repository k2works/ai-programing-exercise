using FluentMigrator.Runner;
using Microsoft.Extensions.DependencyInjection;
using Testcontainers.MySql;
using Xunit;

namespace AccountingSystem.Tests
{
    /// <summary>
    /// データベーステスト用の基底クラス（MySQL版）
    /// </summary>
    public abstract class DatabaseTestBaseMySQL : IAsyncLifetime
    {
        private MySqlContainer? _mysql;
        protected string ConnectionString { get; private set; } = string.Empty;

        public async Task InitializeAsync()
        {
            _mysql = new MySqlBuilder()
                .WithImage("mysql:8.0")
                .WithDatabase("accounting_system_test")
                .WithUsername("test")
                .WithPassword("test")
                .Build();

            await _mysql.StartAsync();
            ConnectionString = _mysql.GetConnectionString();

            // FluentMigratorでマイグレーションを実行
            var serviceProvider = CreateServices();
            using (var scope = serviceProvider.CreateScope())
            {
                UpdateDatabase(scope.ServiceProvider);
            }
        }

        public async Task DisposeAsync()
        {
            if (_mysql != null)
            {
                await _mysql.DisposeAsync();
            }
        }

        private IServiceProvider CreateServices()
        {
            return new ServiceCollection()
                .AddFluentMigratorCore()
                .ConfigureRunner(rb => rb
                    .AddMySql5()
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
