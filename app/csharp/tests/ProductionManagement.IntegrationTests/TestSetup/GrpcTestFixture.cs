using FluentMigrator.Runner;
using Grpc.Net.Client;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.TestHost;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Npgsql;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Infrastructure.Grpc;
using ProductionManagement.Infrastructure.Grpc.Mappers;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using Testcontainers.PostgreSql;

namespace ProductionManagement.IntegrationTests.TestSetup;

/// <summary>
/// gRPC テスト用 Fixture
/// TestServer 上で gRPC サービスを起動し、クライアントからアクセス可能にする
/// </summary>
public class GrpcTestFixture : IAsyncLifetime
{
    private readonly PostgreSqlContainer _postgres = new PostgreSqlBuilder()
        .WithImage("postgres:16-alpine")
        .WithDatabase("testdb")
        .WithUsername("testuser")
        .WithPassword("testpass")
        .Build();

    private IHost? _host;
    private GrpcChannel? _channel;

    public string ConnectionString => _postgres.GetConnectionString();
    public GrpcChannel Channel => _channel ?? throw new InvalidOperationException("Channel not initialized");

    public async Task InitializeAsync()
    {
        // PostgreSQL コンテナ起動
        await _postgres.StartAsync();

        // マイグレーション実行
        await RunMigrationsAsync();

        // gRPC サーバー起動
        _host = await CreateHostAsync();
        await _host.StartAsync();

        // gRPC チャンネル作成
        var httpClient = _host.GetTestClient();
        _channel = GrpcChannel.ForAddress(httpClient.BaseAddress!, new GrpcChannelOptions
        {
            HttpClient = httpClient
        });
    }

    public async Task DisposeAsync()
    {
        _channel?.Dispose();

        if (_host != null)
        {
            await _host.StopAsync();
            _host.Dispose();
        }

        await _postgres.DisposeAsync();
    }

    private async Task RunMigrationsAsync()
    {
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
        await Task.CompletedTask;
    }

    private async Task<IHost> CreateHostAsync()
    {
        var connectionString = ConnectionString;

        var builder = Host.CreateDefaultBuilder()
            .ConfigureWebHostDefaults(webBuilder =>
            {
                webBuilder.UseTestServer();
                webBuilder.ConfigureServices(services =>
                {
                    // リポジトリ登録
                    services.AddScoped<IItemRepository>(_ => new ItemRepository(connectionString));
                    services.AddScoped<IBomRepository>(_ => new BomRepository(connectionString));
                    services.AddScoped<IStockRepository>(_ => new StockRepository(connectionString));
                    services.AddScoped<IPurchaseOrderRepository>(_ => new PurchaseOrderRepository(connectionString));
                    services.AddScoped<IPurchaseOrderDetailRepository>(_ => new PurchaseOrderDetailRepository(connectionString));
                    services.AddScoped<IUnitPriceRepository>(_ => new UnitPriceRepository(connectionString));
                    services.AddScoped<IWorkOrderRepository>(_ => new WorkOrderRepository(connectionString));
                    services.AddScoped<IWorkOrderDetailRepository>(_ => new WorkOrderDetailRepository(connectionString));
                    services.AddScoped<IOrderRepository>(_ => new OrderRepository(connectionString));
                    services.AddScoped<IRoutingRepository>(_ => new RoutingRepository(connectionString));
                    services.AddScoped<IRequirementRepository>(_ => new RequirementRepository(connectionString));
                    services.AddScoped<IAllocationRepository>(_ => new AllocationRepository(connectionString));
                    services.AddScoped<ISupplierRepository>(_ => new SupplierRepository(connectionString));

                    // アプリケーションサービス登録
                    services.AddScoped<IItemUseCase, ItemService>();
                    services.AddScoped<IPurchaseOrderUseCase, PurchaseOrderService>();
                    services.AddScoped<IInventoryUseCase, InventoryService>();
                    services.AddScoped<IWorkOrderUseCase, WorkOrderService>();
                    services.AddScoped<ISupplierUseCase, SupplierService>();
                    services.AddScoped<IOrderUseCase, OrderService>();
                    services.AddScoped<BomService>();
                    services.AddScoped<MrpService>();

                    // gRPC サービス登録
                    services.AddGrpcServices();
                });

                webBuilder.Configure(app =>
                {
                    app.UseRouting();
                    app.UseEndpoints(endpoints =>
                    {
                        endpoints.MapGrpcServices();
                    });
                });
            });

        return await Task.FromResult(builder.Build());
    }

    /// <summary>
    /// テストデータをクリアする
    /// </summary>
    public async Task CleanDatabaseAsync()
    {
        await using var conn = new NpgsqlConnection(ConnectionString);
        await conn.OpenAsync();

        var tables = new[]
        {
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

    /// <summary>
    /// テスト用品目を挿入
    /// </summary>
    public async Task InsertItemAsync(string itemCode, string itemName, ItemCategory category)
    {
        await using var conn = new NpgsqlConnection(ConnectionString);
        await conn.OpenAsync();

        await using var cmd = conn.CreateCommand();
        cmd.CommandText = @"
            INSERT INTO ""品目マスタ"" (
                ""品目コード"", ""適用開始日"", ""品目名称"", ""品目区分"",
                ""リードタイム"", ""安全リードタイム"", ""安全在庫数"",
                ""歩留率"", ""最小ロットサイズ"", ""ロット増分""
            ) VALUES (
                @itemCode, @effectiveFrom, @itemName, @category,
                0, 0, 0, 100, 1, 1
            )";

        cmd.Parameters.AddWithValue("@itemCode", itemCode);
        cmd.Parameters.AddWithValue("@effectiveFrom", DateOnly.FromDateTime(DateTime.Today));
        cmd.Parameters.AddWithValue("@itemName", itemName);
        cmd.Parameters.AddWithValue("@category", category.ToString());

        await cmd.ExecuteNonQueryAsync();
    }

    /// <summary>
    /// テスト用 BOM を挿入
    /// </summary>
    public async Task InsertBomAsync(string parentItemCode, string childItemCode, decimal quantity)
    {
        await using var conn = new NpgsqlConnection(ConnectionString);
        await conn.OpenAsync();

        await using var cmd = conn.CreateCommand();
        cmd.CommandText = @"
            INSERT INTO ""部品構成表"" (
                ""親品目コード"", ""子品目コード"", ""適用開始日"", ""員数""
            ) VALUES (
                @parentCode, @childCode, @effectiveFrom, @quantity
            )";

        cmd.Parameters.AddWithValue("@parentCode", parentItemCode);
        cmd.Parameters.AddWithValue("@childCode", childItemCode);
        cmd.Parameters.AddWithValue("@effectiveFrom", DateOnly.FromDateTime(DateTime.Today));
        cmd.Parameters.AddWithValue("@quantity", quantity);

        await cmd.ExecuteNonQueryAsync();
    }
}

/// <summary>
/// xUnit Collection Definition for gRPC tests
/// </summary>
[CollectionDefinition("Grpc")]
public class GrpcCollection : ICollectionFixture<GrpcTestFixture>
{
}
