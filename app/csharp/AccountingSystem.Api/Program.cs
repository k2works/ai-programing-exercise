using System.Reflection;
using AccountingSystem.Infrastructure.Web.Middleware;
using AccountingSystem.Application.Services;
using AccountingSystem.Domain.Models;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using Dapper;
using Microsoft.OpenApi.Models;
using Npgsql;

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

// Repositories の登録
builder.Services.AddScoped<AccountRepository>(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection")!;
    return new AccountRepository(connectionString);
});
builder.Services.AddScoped<JournalRepository>(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection")!;
    return new JournalRepository(connectionString);
});

// Application Services の登録
builder.Services.AddScoped<IAccountService, AccountService>();
builder.Services.AddScoped<IJournalService, JournalService>();
builder.Services.AddScoped<IFinancialStatementService>(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection")!;
    return new FinancialStatementService(connectionString);
});

// グローバル例外ハンドラーの登録
builder.Services.AddExceptionHandler<GlobalExceptionHandler>();
builder.Services.AddProblemDetails();

var app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI(options =>
    {
        options.SwaggerEndpoint("/swagger/v1/swagger.json", "財務会計システム API v1");
    });
}

// 例外ハンドリングミドルウェア
app.UseExceptionHandler();

app.UseAuthorization();
app.MapControllers();

app.Run();

// テストで WebApplicationFactory を使用するために public に公開
public partial class Program { }
