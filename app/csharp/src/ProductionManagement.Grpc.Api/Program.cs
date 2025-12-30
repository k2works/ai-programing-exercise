using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Infrastructure.Grpc;
using ProductionManagement.Infrastructure.Persistence.Repositories;

var builder = WebApplication.CreateBuilder(args);

// HTTP/2 のみを使用する Kestrel 設定（gRPC 用）
builder.WebHost.ConfigureKestrel(options =>
{
    options.ListenLocalhost(5001, listenOptions =>
    {
        listenOptions.Protocols = Microsoft.AspNetCore.Server.Kestrel.Core.HttpProtocols.Http2;
    });
});

// 接続文字列の取得
var connectionString = builder.Configuration.GetConnectionString("DefaultConnection")
    ?? throw new InvalidOperationException("Connection string 'DefaultConnection' not found.");

// リポジトリの登録
builder.Services.AddScoped<IItemRepository>(_ => new ItemRepository(connectionString));
builder.Services.AddScoped<IBomRepository>(_ => new BomRepository(connectionString));
builder.Services.AddScoped<IStockRepository>(_ => new StockRepository(connectionString));
builder.Services.AddScoped<IPurchaseOrderRepository>(_ => new PurchaseOrderRepository(connectionString));
builder.Services.AddScoped<IPurchaseOrderDetailRepository>(_ => new PurchaseOrderDetailRepository(connectionString));
builder.Services.AddScoped<IUnitPriceRepository>(_ => new UnitPriceRepository(connectionString));
builder.Services.AddScoped<IWorkOrderRepository>(_ => new WorkOrderRepository(connectionString));
builder.Services.AddScoped<IWorkOrderDetailRepository>(_ => new WorkOrderDetailRepository(connectionString));
builder.Services.AddScoped<IOrderRepository>(_ => new OrderRepository(connectionString));
builder.Services.AddScoped<IRoutingRepository>(_ => new RoutingRepository(connectionString));
builder.Services.AddScoped<IRequirementRepository>(_ => new RequirementRepository(connectionString));
builder.Services.AddScoped<IAllocationRepository>(_ => new AllocationRepository(connectionString));
builder.Services.AddScoped<ISupplierRepository>(_ => new SupplierRepository(connectionString));

// アプリケーションサービスの登録
builder.Services.AddScoped<IItemUseCase, ItemService>();
builder.Services.AddScoped<BomService>();
builder.Services.AddScoped<IPurchaseOrderUseCase, PurchaseOrderService>();
builder.Services.AddScoped<IInventoryUseCase, InventoryService>();
builder.Services.AddScoped<IWorkOrderUseCase, WorkOrderService>();
builder.Services.AddScoped<MrpService>();
builder.Services.AddScoped<ISupplierUseCase, SupplierService>();
builder.Services.AddScoped<IOrderUseCase, OrderService>();

// gRPC サービスの登録
builder.Services.AddGrpcServices();

var app = builder.Build();

// gRPC エンドポイントのマッピング
app.MapGrpcServices();
app.MapGrpcReflectionIfDevelopment(app.Environment.IsDevelopment());

await app.RunAsync();

// テストからアクセス可能にするため
public partial class Program { }
