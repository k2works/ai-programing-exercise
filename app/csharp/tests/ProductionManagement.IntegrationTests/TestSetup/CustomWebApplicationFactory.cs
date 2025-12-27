using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.AspNetCore.TestHost;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Infrastructure.Persistence.Repositories;

namespace ProductionManagement.IntegrationTests.TestSetup;

/// <summary>
/// カスタム WebApplicationFactory - テスト用の API サーバーを構築
/// </summary>
public class CustomWebApplicationFactory : WebApplicationFactory<Program>
{
    private readonly string _connectionString;

    public CustomWebApplicationFactory(string connectionString)
    {
        _connectionString = connectionString;
    }

    protected override void ConfigureWebHost(IWebHostBuilder builder)
    {
        // 環境変数でテスト用の接続文字列を設定（最優先で読み込まれる）
        builder.UseSetting("ConnectionStrings:DefaultConnection", _connectionString);

        // ConfigureTestServices は Program.cs のサービス登録後に実行される
        builder.ConfigureTestServices(services =>
        {
            // 既存のリポジトリ登録を削除して再登録
            var serviceTypesToRemove = new[]
            {
                typeof(IItemRepository),
                typeof(IBomRepository),
                typeof(IStockRepository),
                typeof(IPurchaseOrderRepository),
                typeof(IPurchaseOrderDetailRepository),
                typeof(IUnitPriceRepository),
                typeof(IWorkOrderRepository),
                typeof(IWorkOrderDetailRepository),
                typeof(IOrderRepository),
                typeof(IRoutingRepository),
                typeof(IRequirementRepository),
                typeof(IAllocationRepository),
                typeof(ISupplierRepository),
                typeof(IItemUseCase),
                typeof(IPurchaseOrderUseCase),
                typeof(IInventoryUseCase),
                typeof(IWorkOrderUseCase),
                typeof(ISupplierUseCase),
                typeof(IOrderUseCase),
                typeof(BomService),
                typeof(MrpService)
            };

            var descriptorsToRemove = services
                .Where(d => serviceTypesToRemove.Contains(d.ServiceType))
                .ToList();

            foreach (var descriptor in descriptorsToRemove)
            {
                services.Remove(descriptor);
            }

            // テスト用の接続文字列でリポジトリを再登録
            services.AddScoped<IItemRepository>(_ => new ItemRepository(_connectionString));
            services.AddScoped<IBomRepository>(_ => new BomRepository(_connectionString));
            services.AddScoped<IStockRepository>(_ => new StockRepository(_connectionString));
            services.AddScoped<IPurchaseOrderRepository>(_ => new PurchaseOrderRepository(_connectionString));
            services.AddScoped<IPurchaseOrderDetailRepository>(_ => new PurchaseOrderDetailRepository(_connectionString));
            services.AddScoped<IUnitPriceRepository>(_ => new UnitPriceRepository(_connectionString));
            services.AddScoped<IWorkOrderRepository>(_ => new WorkOrderRepository(_connectionString));
            services.AddScoped<IWorkOrderDetailRepository>(_ => new WorkOrderDetailRepository(_connectionString));
            services.AddScoped<IOrderRepository>(_ => new OrderRepository(_connectionString));
            services.AddScoped<IRoutingRepository>(_ => new RoutingRepository(_connectionString));
            services.AddScoped<IRequirementRepository>(_ => new RequirementRepository(_connectionString));
            services.AddScoped<IAllocationRepository>(_ => new AllocationRepository(_connectionString));
            services.AddScoped<ISupplierRepository>(_ => new SupplierRepository(_connectionString));

            // アプリケーションサービスを再登録
            services.AddScoped<IItemUseCase, ItemService>();
            services.AddScoped<IPurchaseOrderUseCase, PurchaseOrderService>();
            services.AddScoped<IInventoryUseCase, InventoryService>();
            services.AddScoped<IWorkOrderUseCase, WorkOrderService>();
            services.AddScoped<ISupplierUseCase, SupplierService>();
            services.AddScoped<IOrderUseCase, OrderService>();
            services.AddScoped<BomService>();
            services.AddScoped<MrpService>();

            // テスト用にProblemDetailsの詳細を有効化
            services.AddProblemDetails(options =>
            {
                options.CustomizeProblemDetails = context =>
                {
                    context.ProblemDetails.Extensions["exception"] = context.Exception?.ToString();
                };
            });
        });

        builder.UseEnvironment("Development");
    }
}
