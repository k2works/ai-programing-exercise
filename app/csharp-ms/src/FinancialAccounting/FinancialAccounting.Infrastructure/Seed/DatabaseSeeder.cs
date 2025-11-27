using FluentMigrator.Runner;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

namespace FinancialAccounting.Infrastructure.Seed;

/// <summary>
/// データベースシーダー（初回起動時のシードデータ投入）
/// </summary>
public class DatabaseSeeder : IHostedService
{
    private readonly IServiceProvider _serviceProvider;
    private readonly IConfiguration _configuration;
    private readonly ILogger<DatabaseSeeder> _logger;

    public DatabaseSeeder(
        IServiceProvider serviceProvider,
        IConfiguration configuration,
        ILogger<DatabaseSeeder> logger)
    {
        _serviceProvider = serviceProvider;
        _configuration = configuration;
        _logger = logger;
    }

    public Task StartAsync(CancellationToken cancellationToken)
    {
        var enableSeedData = _configuration.GetValue<bool>("Seeding:EnableSeedData");

        if (!enableSeedData)
        {
            _logger.LogInformation("Seed data loading is disabled");
            return Task.CompletedTask;
        }

        _logger.LogInformation("Starting seed data loading...");

        using var scope = _serviceProvider.CreateScope();
        var runner = scope.ServiceProvider.GetRequiredService<IMigrationRunner>();

        // シードデータマイグレーション (V099) まで実行
        runner.MigrateUp(99);

        _logger.LogInformation("Seed data loading completed");
        return Task.CompletedTask;
    }

    public Task StopAsync(CancellationToken cancellationToken) => Task.CompletedTask;
}
