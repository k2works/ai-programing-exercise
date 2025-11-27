using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Application.Services;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using Dapper;
using FluentMigrator.Runner;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.DependencyInjection.Extensions;
using Microsoft.Extensions.Logging;
using Npgsql;
using Testcontainers.PostgreSql;
using Xunit;

namespace AccountingSystem.Tests.Integration;

/// <summary>
/// API統合テスト用の基底クラス
/// DatabaseTestBaseを拡張し、WebApplicationFactoryを提供する
/// </summary>
public abstract class ApiTestBase : IAsyncLifetime
{
    private PostgreSqlContainer? _postgres;
    private WebApplicationFactory<Program>? _factory;

    protected string ConnectionString { get; private set; } = string.Empty;
    protected HttpClient Client { get; private set; } = null!;

    static ApiTestBase()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public async Task InitializeAsync()
    {
        // TestContainers で PostgreSQL を起動
        _postgres = new PostgreSqlBuilder()
            .WithImage("postgres:16-alpine")
            .WithDatabase("accounting_system_test")
            .WithUsername("test")
            .WithPassword("test")
            .Build();

        await _postgres.StartAsync();
        ConnectionString = _postgres.GetConnectionString();

        // FluentMigrator でマイグレーション実行
        var serviceProvider = CreateMigrationServices();
        using (var scope = serviceProvider.CreateScope())
        {
            var runner = scope.ServiceProvider.GetRequiredService<IMigrationRunner>();
            runner.MigrateUp();
        }

        // サブクラスの初期化処理（テストデータ投入など）
        await OnInitializedAsync();

        // WebApplicationFactory で API サーバーを起動
        _factory = new WebApplicationFactory<Program>()
            .WithWebHostBuilder(builder =>
            {
                builder.UseSetting(
                    "ConnectionStrings:DefaultConnection",
                    ConnectionString);

                builder.ConfigureServices(services =>
                {
                    // テスト用のモック IEventPublisher を登録
                    services.RemoveAll<IEventPublisher>();
                    services.AddSingleton<IEventPublisher, NoOpEventPublisher>();

                    // IMonthlyAccountBalanceRepository を登録
                    services.RemoveAll<IMonthlyAccountBalanceRepository>();
                    services.AddScoped<IMonthlyAccountBalanceRepository>(sp =>
                        new MonthlyAccountBalanceRepository(ConnectionString));

                    // IFinancialAnalysisService を登録
                    services.RemoveAll<IFinancialAnalysisService>();
                    services.AddScoped<IFinancialAnalysisService, FinancialAnalysisService>();

                    // JournalEntryEventSourcingServiceWithEventBus を登録
                    services.AddScoped<JournalEntryEventSourcingServiceWithEventBus>(sp =>
                    {
                        var innerService = sp.GetRequiredService<IJournalEntryEventSourcingService>();
                        var eventPublisher = sp.GetRequiredService<IEventPublisher>();
                        var logger = sp.GetRequiredService<ILogger<JournalEntryEventSourcingServiceWithEventBus>>();
                        return new JournalEntryEventSourcingServiceWithEventBus(innerService, eventPublisher, logger);
                    });
                });
            });

        Client = _factory.CreateClient();
    }

    /// <summary>
    /// サブクラスでオーバーライドして初期化処理（テストデータ投入など）を追加
    /// </summary>
    protected virtual Task OnInitializedAsync() => Task.CompletedTask;

    public async Task DisposeAsync()
    {
        Client?.Dispose();
        _factory?.Dispose();

        if (_postgres != null)
        {
            await _postgres.DisposeAsync();
        }
    }

    private IServiceProvider CreateMigrationServices()
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

    /// <summary>
    /// テストデータ投入用のヘルパーメソッド
    /// </summary>
    protected async Task ExecuteSqlAsync(string sql, object? param = null)
    {
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await connection.ExecuteAsync(sql, param);
    }
}

/// <summary>
/// テスト用の何もしない IEventPublisher 実装
/// </summary>
internal class NoOpEventPublisher : IEventPublisher
{
    public Task PublishAsync<TEvent>(TEvent @event, string routingKey)
    {
        // 何もしない（テスト用）
        return Task.CompletedTask;
    }
}
