using FluentMigrator.Runner;
using ManagementAccounting.Application.Ports.In;
using ManagementAccounting.Application.Ports.Out;
using ManagementAccounting.Application.UseCases;
using ManagementAccounting.Infrastructure.Adapters.External;
using ManagementAccounting.Infrastructure.Messaging;
using ManagementAccounting.Infrastructure.Persistence;
using ManagementAccounting.Infrastructure.Persistence.Migrations;
using MassTransit;
using Refit;

var builder = WebApplication.CreateBuilder(args);

// Add services to the container
builder.Services.AddControllers();
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

// Use Cases
builder.Services.AddScoped<IAnalyzeFinancialDataUseCase, AnalyzeFinancialDataUseCase>();

// Repository
builder.Services.AddScoped<IFinancialAnalysisCacheRepository, FinancialAnalysisCacheRepository>();

// 腐敗防止層: 財務会計サービスクライアント (Refit による型安全な HTTP クライアント)
builder.Services
    .AddRefitClient<IFinancialAccountingApi>()
    .ConfigureHttpClient(c =>
    {
        var baseUrl = builder.Configuration["FinancialAccountingService:BaseUrl"]
            ?? "http://financial-accounting-service:8080";
        c.BaseAddress = new Uri(baseUrl);
        c.DefaultRequestHeaders.Add("Accept", "application/json");
    });
builder.Services.AddScoped<IFinancialAccountingClient, FinancialAccountingClient>();

// FluentMigrator
builder.Services
    .AddFluentMigratorCore()
    .ConfigureRunner(rb => rb
        .AddPostgres()
        .WithGlobalConnectionString(builder.Configuration.GetConnectionString("ManagementAccounting"))
        .ScanIn(typeof(V001_CreateFinancialAnalysisCache).Assembly).For.Migrations())
    .AddLogging(lb => lb.AddFluentMigratorConsole());

// MassTransit + RabbitMQ (イベント消費)
builder.Services.AddMassTransit(x =>
{
    x.AddConsumer<JournalCreatedEventConsumer>();

    x.UsingRabbitMq((context, cfg) =>
    {
        var host = builder.Configuration["RabbitMQ:Host"] ?? "localhost";
        var username = builder.Configuration["RabbitMQ:Username"] ?? "guest";
        var password = builder.Configuration["RabbitMQ:Password"] ?? "guest";

        cfg.Host(host, "/", h =>
        {
            h.Username(username);
            h.Password(password);
        });

        cfg.ConfigureEndpoints(context);
    });
});

// CORS
builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowAll", policy =>
    {
        policy.AllowAnyOrigin()
              .AllowAnyMethod()
              .AllowAnyHeader();
    });
});

var app = builder.Build();

// Configure the HTTP request pipeline
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseCors("AllowAll");
app.UseAuthorization();
app.MapControllers();

// Run migrations
using (var scope = app.Services.CreateScope())
{
    var runner = scope.ServiceProvider.GetRequiredService<IMigrationRunner>();
    runner.MigrateUp();
}

app.Run();

// テストで WebApplicationFactory を使用するために public に公開
public partial class Program { }
