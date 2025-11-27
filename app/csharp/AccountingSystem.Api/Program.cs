using MediatR;
using System.Reflection;
using AccountingSystem.Infrastructure.Web.Middleware;
using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Application.Services;
using AccountingSystem.Domain.Models.Financial;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using AccountingSystem.Infrastructure;
using AccountingSystem.Infrastructure.Seed;
using AccountingSystem.Infrastructure.EventHandlers;
using AccountingSystem.Infrastructure.EventBus;
using Dapper;
using Microsoft.OpenApi.Models;
using Npgsql;

// マイグレーションコマンドの処理
if (args.Length > 0 && args[0].Equals("migrate", StringComparison.OrdinalIgnoreCase))
{
    var configuration = new ConfigurationBuilder()
        .SetBasePath(Directory.GetCurrentDirectory())
        .AddJsonFile("appsettings.json", optional: false)
        .AddJsonFile($"appsettings.{Environment.GetEnvironmentVariable("ASPNETCORE_ENVIRONMENT") ?? "Production"}.json", optional: true)
        .AddEnvironmentVariables()
        .Build();

    var databaseType = configuration["DatabaseType"] ?? "PostgreSQL";
    var connectionString = configuration.GetConnectionString("DefaultConnection");

    if (string.IsNullOrEmpty(connectionString))
    {
        Console.WriteLine($"接続文字列が見つかりません: DefaultConnection");
        return 1;
    }

    Console.WriteLine($"データベースマイグレーション開始: {databaseType}");
    MigrationRunner.MigrateDatabase(connectionString, databaseType);
    Console.WriteLine("マイグレーション完了");
    return 0;
}

var builder = WebApplication.CreateBuilder(args);

// Add services to the container.
builder.Services.AddControllers();

// Swagger/OpenAPI の設定
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(options =>
{
    options.SwaggerDoc("v1", new OpenApiInfo
    {
        Title = "財務会計システム API",
        Version = "v1",
        Description = "財務会計システムの REST API ドキュメント",
        Contact = new OpenApiContact
        {
            Name = "開発チーム",
            Email = "dev@example.com"
        }
    });

    // XML コメントを有効化
    var xmlFile = $"{Assembly.GetExecutingAssembly().GetName().Name}.xml";
    var xmlPath = Path.Combine(AppContext.BaseDirectory, xmlFile);
    if (File.Exists(xmlPath))
    {
        options.IncludeXmlComments(xmlPath);
    }
});

// Dapper の DateOnly 型ハンドラー登録
SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());

// データベース接続
builder.Services.AddScoped<NpgsqlConnection>(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection");
    return new NpgsqlConnection(connectionString);
});

// Repositories の登録（出力ポート）
builder.Services.AddScoped<IAccountRepository>(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection")!;
    return new AccountRepository(connectionString);
});
builder.Services.AddScoped<IJournalRepository>(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection")!;
    return new JournalRepository(connectionString);
});

// 監査ログリポジトリの登録
builder.Services.AddScoped<IAuditLogRepository>(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection")!;
    return new AuditLogRepository(connectionString);
});

// MediatR の登録（ドメインイベント）
builder.Services.AddMediatR(cfg =>
{
    cfg.RegisterServicesFromAssembly(typeof(Program).Assembly);
    // Infrastructure アセンブリのイベントハンドラー（AuditEventHandler 等）を登録
    cfg.RegisterServicesFromAssembly(typeof(AuditEventHandler).Assembly);
});

// Event Sourcing リポジトリの登録
builder.Services.AddScoped<IEventStoreRepository>(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection")!;
    return new EventStoreRepository(connectionString);
});

// 月次勘定科目残高リポジトリの登録
builder.Services.AddScoped<IMonthlyAccountBalanceRepository>(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection")!;
    return new MonthlyAccountBalanceRepository(connectionString);
});

// 仕訳 Read Model リポジトリの登録（CQRS Query Side）
builder.Services.AddScoped<IJournalEntryReadModelRepository>(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection")!;
    return new JournalEntryReadModelRepository(connectionString);
});

// Application Services の登録（入力ポート）
builder.Services.AddScoped<IAccountService, AccountService>();
builder.Services.AddScoped<IJournalService, JournalService>();
builder.Services.AddScoped<IAuditLogService, AuditLogService>();
builder.Services.AddScoped<IFinancialStatementService>(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection")!;
    return new FinancialStatementService(connectionString);
});
builder.Services.AddScoped<IFinancialAnalysisService, FinancialAnalysisService>();
builder.Services.AddScoped<IJournalEntryEventSourcingService, JournalEntryEventSourcingService>();

// RabbitMQ イベントバスの登録（IEventPublisher を提供）
builder.Services.AddRabbitMQEventBus();

// イベントソーシング + イベントバス連携サービスの登録
builder.Services.AddJournalEntryEventSourcingServiceWithEventBus();

// データベース Seed サービスの登録
builder.Services.AddHostedService<DatabaseSeeder>();

// グローバル例外ハンドラーの登録
builder.Services.AddExceptionHandler<GlobalExceptionHandler>();
builder.Services.AddProblemDetails();

// CORS 設定（Script Lab からのアクセスを許可）
builder.Services.AddCors(options =>
{
    options.AddPolicy("ScriptLabPolicy", policy =>
    {
        policy
            // Script Lab、Excel Online、開発環境からのアクセスを許可
            .WithOrigins(
                "https://script-lab.azureedge.net",    // Script Lab
                "https://script-lab-react.azureedge.net", // Script Lab React
                "https://*.cdn.office.net",           // Office CDN
                "https://*.officeapps.live.com",      // Excel Online
                "https://*.office.com",               // Office 365
                "http://localhost:5000",              // ローカル開発
                "http://localhost:5001",              // ローカル開発 HTTPS
                "https://localhost:5001"              // ローカル開発 HTTPS
            )
            .SetIsOriginAllowedToAllowWildcardSubdomains()
            .AllowAnyMethod()
            .AllowAnyHeader()
            .AllowCredentials();
    });

    // 開発環境用の緩い CORS ポリシー
    options.AddPolicy("DevelopmentPolicy", policy =>
    {
        policy
            .AllowAnyOrigin()
            .AllowAnyMethod()
            .AllowAnyHeader();
    });
});

var app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI(options =>
    {
        options.SwaggerEndpoint("/swagger/v1/swagger.json", "財務会計システム API v1");
    });

    // 開発環境では緩い CORS ポリシーを使用
    app.UseCors("DevelopmentPolicy");
}
else
{
    // 本番環境では Script Lab ポリシーを使用
    app.UseCors("ScriptLabPolicy");
}

// 例外ハンドリングミドルウェア
app.UseExceptionHandler();

app.UseAuthorization();
app.MapControllers();

app.Run();
return 0;

// テストで WebApplicationFactory を使用するために public に公開
public partial class Program { }
