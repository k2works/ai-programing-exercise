using Microsoft.AspNetCore.Mvc.ApplicationParts;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Web.Controllers;
using ProductionManagement.Web.Filters;

var builder = WebApplication.CreateBuilder(args);

// 接続文字列の取得
var connectionString = builder.Configuration.GetConnectionString("DefaultConnection")
    ?? throw new InvalidOperationException("Connection string 'DefaultConnection' not found.");

// リポジトリの登録（API 版と共通）
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

// アプリケーションサービスの登録（API 版と共通）
builder.Services.AddScoped<IItemUseCase, ItemService>();
builder.Services.AddScoped<BomService>();
builder.Services.AddScoped<IPurchaseOrderUseCase, PurchaseOrderService>();
builder.Services.AddScoped<IInventoryUseCase, InventoryService>();
builder.Services.AddScoped<IWorkOrderUseCase, WorkOrderService>();
builder.Services.AddScoped<MrpService>();
builder.Services.AddScoped<ISupplierUseCase, SupplierService>();
builder.Services.AddScoped<IOrderUseCase, OrderService>();

// ドメイン例外フィルターの登録
builder.Services.AddScoped<DomainExceptionFilter>();

// MVC + Razor Views（Web プロジェクトのコントローラーのみ登録）
builder.Services.AddControllersWithViews(options =>
    {
        // ドメイン例外フィルターをグローバルに適用
        options.Filters.Add<DomainExceptionFilter>();
    })
    .ConfigureApplicationPartManager(manager =>
    {
        // Infrastructure の REST コントローラーを除外
        var partsToRemove = manager.ApplicationParts
            .Where(p => p.Name == "ProductionManagement.Infrastructure")
            .ToList();
        foreach (var part in partsToRemove)
        {
            manager.ApplicationParts.Remove(part);
        }
    });

// Session
builder.Services.AddDistributedMemoryCache();
builder.Services.AddSession(options =>
{
    options.IdleTimeout = TimeSpan.FromMinutes(30);
    options.Cookie.HttpOnly = true;
    options.Cookie.IsEssential = true;
});

var app = builder.Build();

// Configure the HTTP request pipeline.
if (!app.Environment.IsDevelopment())
{
    app.UseExceptionHandler("/Home/Error");
    app.UseHsts();
}

app.UseHttpsRedirection();
app.UseStaticFiles();
app.UseRouting();
app.UseSession();
app.UseAuthorization();

app.MapControllerRoute(
    name: "default",
    pattern: "{controller=Home}/{action=Index}/{id?}");

app.Run();

// テストからアクセス可能にするため
public partial class Program { }
