using System.Data.Common;
using System.IO;
using System.Windows;
using System.Windows.Threading;
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
using ProductionManagement.WPF.ViewModels.Inventory;
using ProductionManagement.WPF.ViewModels.Planning;
using ProductionManagement.WPF.ViewModels.Suppliers;
using ProductionManagement.WPF.ViewModels.WorkOrders;
using ProductionManagement.WPF.ViewModels.Reports;
using ProductionManagement.WPF.ViewModels.Grpc;

namespace ProductionManagement.WPF;

/// <summary>
/// WPF アプリケーション エントリポイント
/// </summary>
public partial class App : System.Windows.Application
{
    private readonly IHost _host;
    private bool _isHandlingException;

    static App()
    {
        // HTTP/2 非暗号化サポートを有効化（gRPC で HTTP を使用するために必要）
        // 静的コンストラクタで最初に設定する必要がある
        AppContext.SetSwitch("System.Net.Http.SocketsHttpHandler.Http2UnencryptedSupport", true);
    }

    public App()
    {
        // グローバル例外ハンドラを登録
        DispatcherUnhandledException += OnDispatcherUnhandledException;
        AppDomain.CurrentDomain.UnhandledException += OnUnhandledException;
        TaskScheduler.UnobservedTaskException += OnUnobservedTaskException;

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
        services.AddTransient<IPurchaseOrderDetailRepository>(_ => new PurchaseOrderDetailRepository(connectionString));
        services.AddTransient<IUnitPriceRepository>(_ => new UnitPriceRepository(connectionString));
        services.AddTransient<IWorkOrderRepository>(_ => new WorkOrderRepository(connectionString));
        services.AddTransient<IStockRepository>(_ => new StockRepository(connectionString));
        services.AddTransient<IOrderRepository>(_ => new OrderRepository(connectionString));
        services.AddTransient<ILocationRepository>(_ => new LocationRepository(connectionString));

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
        services.AddSingleton<IReportService, ExcelReportService>();

        // gRPC Services
        services.AddSingleton<IGrpcChannelFactory>(sp =>
            new GrpcChannelFactory(configuration));

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
        services.AddTransient<MrpExecuteViewModel>();
        services.AddTransient<OrderListViewModel>();
        services.AddTransient<WorkOrderListViewModel>();
        services.AddTransient<StockListViewModel>();
        services.AddTransient<ReportListViewModel>();

        // gRPC ViewModels
        services.AddTransient<GrpcItemListViewModel>();
        services.AddTransient<GrpcBomExplodeViewModel>();
        services.AddTransient<GrpcMrpViewModel>();

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

    /// <summary>
    /// UI スレッドでの未処理例外ハンドラ
    /// </summary>
    private void OnDispatcherUnhandledException(object sender, DispatcherUnhandledExceptionEventArgs e)
    {
        // 再帰的な例外処理を防止
        if (_isHandlingException)
        {
            e.Handled = true;
            return;
        }

        try
        {
            _isHandlingException = true;

            var message = GetUserFriendlyMessage(e.Exception);
            LogException(e.Exception);

            MessageBox.Show(
                message,
                "エラー",
                MessageBoxButton.OK,
                MessageBoxImage.Error);

            // 回復可能な例外の場合はアプリ継続
            e.Handled = IsRecoverableException(e.Exception);
        }
        finally
        {
            _isHandlingException = false;
        }
    }

    /// <summary>
    /// 非 UI スレッドでの未処理例外ハンドラ
    /// </summary>
    private void OnUnhandledException(object sender, UnhandledExceptionEventArgs e)
    {
        if (e.ExceptionObject is Exception ex)
        {
            var message = GetUserFriendlyMessage(ex);
            LogException(ex);

            MessageBox.Show(
                $"{message}\n\nアプリケーションを終了します。",
                "致命的エラー",
                MessageBoxButton.OK,
                MessageBoxImage.Error);
        }
    }

    /// <summary>
    /// Task の未観測例外ハンドラ
    /// </summary>
    private void OnUnobservedTaskException(object? sender, UnobservedTaskExceptionEventArgs e)
    {
        LogException(e.Exception);

        // 例外を観測済みにしてアプリ継続
        e.SetObserved();

        Dispatcher.Invoke(() =>
        {
            var message = GetUserFriendlyMessage(e.Exception);
            MessageBox.Show(
                message,
                "バックグラウンドエラー",
                MessageBoxButton.OK,
                MessageBoxImage.Warning);
        });
    }

    /// <summary>
    /// ユーザーフレンドリーなメッセージを取得
    /// </summary>
    private static string GetUserFriendlyMessage(Exception ex)
    {
        return ex switch
        {
            DbException => "データベース接続に問題が発生しました。接続設定を確認してください。",
            IOException => "ファイルの読み書きに問題が発生しました。ファイルが使用中でないか確認してください。",
            UnauthorizedAccessException => "アクセス権限がありません。管理者権限で実行してください。",
            TimeoutException => "処理がタイムアウトしました。しばらく待ってから再度お試しください。",
            InvalidOperationException ioe when ioe.Message.Contains("接続") =>
                "データベース接続に問題が発生しました。接続設定を確認してください。",
            AggregateException ae => GetUserFriendlyMessage(ae.InnerException ?? ae),
            _ => $"予期しないエラーが発生しました。\n\n{ex.Message}"
        };
    }

    /// <summary>
    /// 回復可能な例外かどうかを判定
    /// </summary>
    private static bool IsRecoverableException(Exception ex)
    {
        return ex is not (
            OutOfMemoryException or
            StackOverflowException or
            AccessViolationException);
    }

    /// <summary>
    /// 例外をログに記録
    /// </summary>
    private static void LogException(Exception ex)
    {
        try
        {
            var logPath = Path.Combine(
                AppDomain.CurrentDomain.BaseDirectory,
                "logs",
                $"error_{DateTime.Now:yyyyMMdd}.log");

            Directory.CreateDirectory(Path.GetDirectoryName(logPath)!);

            var logMessage = $"""
                [{DateTime.Now:yyyy-MM-dd HH:mm:ss}]
                Type: {ex.GetType().FullName}
                Message: {ex.Message}
                StackTrace:
                {ex.StackTrace}
                ---
                """;

            File.AppendAllText(logPath, logMessage);
        }
        catch
        {
            // ログ記録失敗は無視
        }
    }
}
