using ProductionManagement.Api.Configuration;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Infrastructure.Grpc;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Infrastructure.Rest.Controllers;
using ProductionManagement.Infrastructure.Rest.Middleware;

var builder = WebApplication.CreateBuilder(args);

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

// CORS 設定
builder.Services.AddCors(options =>
{
    options.AddDefaultPolicy(policy =>
    {
        policy.AllowAnyOrigin()
              .AllowAnyMethod()
              .AllowAnyHeader();
    });
});

// コントローラと例外ハンドラ
// Infrastructure プロジェクトのコントローラを登録
builder.Services.AddControllers()
    .AddApplicationPart(typeof(RootController).Assembly);
builder.Services.AddProblemDetails();

// OpenAPI / Swagger
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(OpenApiConfiguration.ConfigureSwagger);

// gRPC サービス
builder.Services.AddGrpcServices();

var app = builder.Build();

// 例外ハンドラ
app.ConfigureExceptionHandler();
app.UseMiddleware<DomainExceptionMiddleware>();

// Swagger UI
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI(options =>
    {
        options.SwaggerEndpoint("/swagger/v1/swagger.json", "生産管理システム API v1");
    });
}

app.UseHttpsRedirection();
app.UseCors();
app.UseAuthorization();
app.MapControllers();

// gRPC エンドポイント
app.MapGrpcServices();
app.MapGrpcReflectionIfDevelopment(app.Environment.IsDevelopment());

await app.RunAsync();

// テストからアクセス可能にするため
public partial class Program { }
