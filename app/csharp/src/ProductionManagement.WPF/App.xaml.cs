using System.Windows;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.WPF.Services;
using ProductionManagement.WPF.ViewModels;
using ProductionManagement.WPF.ViewModels.Bom;
using ProductionManagement.WPF.ViewModels.Items;
using ProductionManagement.WPF.ViewModels.PurchaseOrders;
using ProductionManagement.WPF.ViewModels.Suppliers;

namespace ProductionManagement.WPF;

/// <summary>
/// WPF アプリケーション エントリポイント
/// </summary>
public partial class App : System.Windows.Application
{
    private readonly IHost _host;

    public App()
    {
        _host = Host.CreateDefaultBuilder()
            .ConfigureAppConfiguration((context, config) =>
            {
                config.SetBasePath(AppDomain.CurrentDomain.BaseDirectory);
                config.AddJsonFile("appsettings.json", optional: true, reloadOnChange: true);
            })
            .ConfigureServices((context, services) =>
            {
                ConfigureServices(services, context.Configuration);
            })
            .Build();
    }

    /// <summary>
    /// サービスプロバイダー（View から ViewModel を取得する用）
    /// </summary>
    public static IServiceProvider Services => ((App)Current)._host.Services;

    private static void ConfigureServices(IServiceCollection services, IConfiguration configuration)
    {
        // Database connection
        var connectionString = configuration.GetConnectionString("DefaultConnection")
            ?? "Host=localhost;Database=production;Username=postgres;Password=postgres";

        // Repository（Output Ports）- connectionString を登録
        services.AddTransient<IItemRepository>(_ => new ItemRepository(connectionString));
        services.AddTransient<IBomRepository>(_ => new BomRepository(connectionString));
        services.AddTransient<ISupplierRepository>(_ => new SupplierRepository(connectionString));
        services.AddTransient<IPurchaseOrderRepository>(_ => new PurchaseOrderRepository(connectionString));
        services.AddTransient<IWorkOrderRepository>(_ => new WorkOrderRepository(connectionString));
        services.AddTransient<IStockRepository>(_ => new StockRepository(connectionString));

        // Application Services（Input Ports）
        services.AddTransient<IItemUseCase, ItemService>();
        services.AddTransient<ISupplierUseCase, SupplierService>();
        services.AddTransient<IPurchaseOrderUseCase, PurchaseOrderService>();
        services.AddTransient<IWorkOrderUseCase, WorkOrderService>();
        services.AddTransient<IInventoryUseCase, InventoryService>();
        services.AddTransient<BomService>();

        // UI Services
        services.AddSingleton<INavigationService, NavigationService>();
        services.AddSingleton<IDialogService, DialogService>();

        // ViewModels
        services.AddTransient<MainViewModel>();
        services.AddTransient<ItemListViewModel>();
        services.AddTransient<ItemDetailViewModel>();
        services.AddTransient<ItemEditViewModel>();
        services.AddTransient<BomExplodeViewModel>();
        services.AddTransient<SupplierListViewModel>();
        services.AddTransient<SupplierDetailViewModel>();
        services.AddTransient<SupplierEditViewModel>();
        services.AddTransient<PurchaseOrderListViewModel>();
        services.AddTransient<PurchaseOrderEditViewModel>();

        // Main Window
        services.AddSingleton<MainWindow>();
    }

    private async void Application_Startup(object sender, StartupEventArgs e)
    {
        await _host.StartAsync();

        var mainWindow = _host.Services.GetRequiredService<MainWindow>();
        mainWindow.DataContext = _host.Services.GetRequiredService<MainViewModel>();
        mainWindow.Show();
    }

    private async void Application_Exit(object sender, ExitEventArgs e)
    {
        await _host.StopAsync();
        _host.Dispose();
    }
}
