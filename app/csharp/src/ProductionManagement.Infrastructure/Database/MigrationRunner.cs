using FluentMigrator.Runner;
using Microsoft.Extensions.DependencyInjection;

namespace ProductionManagement.Infrastructure.Database;

/// <summary>
/// FluentMigrator マイグレーションランナー
/// </summary>
public static class MigrationRunner
{
    /// <summary>
    /// マイグレーションを実行する
    /// </summary>
    /// <param name="connectionString">データベース接続文字列</param>
    public static void RunMigrations(string connectionString)
    {
        var serviceProvider = CreateServices(connectionString);

        using var scope = serviceProvider.CreateScope();
        UpdateDatabase(scope.ServiceProvider);
    }

    private static IServiceProvider CreateServices(string connectionString)
    {
        return new ServiceCollection()
            .AddFluentMigratorCore()
            .ConfigureRunner(rb => rb
                .AddPostgres()
                .WithGlobalConnectionString(connectionString)
                .ScanIn(typeof(MigrationRunner).Assembly).For.Migrations())
            .AddLogging(lb => lb.AddFluentMigratorConsole())
            .BuildServiceProvider(false);
    }

    private static void UpdateDatabase(IServiceProvider serviceProvider)
    {
        var runner = serviceProvider.GetRequiredService<IMigrationRunner>();
        runner.MigrateUp();
    }
}
