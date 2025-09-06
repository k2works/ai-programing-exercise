using MRS.Api.Middleware;
using MRS.Api.Services;
using MRS.Application.Ports;
using MRS.Application.Services;
using MRS.Infrastructure.Data;
using MRS.Infrastructure.Repositories;
using Microsoft.OpenApi.Models;
using OpenTelemetry.Metrics;
using Serilog;
using Serilog.Formatting.Compact;

var builder = WebApplication.CreateBuilder(args);

// Configure Serilog
Log.Logger = new LoggerConfiguration()
    .ReadFrom.Configuration(builder.Configuration)
    .Enrich.FromLogContext()
    .Enrich.WithEnvironmentName()
    .Enrich.WithMachineName()
    .Enrich.WithProcessId()
    .Enrich.WithThreadId()
    .WriteTo.Console(new CompactJsonFormatter())
    .WriteTo.File(
        new CompactJsonFormatter(),
        path: "logs/mrs-.log",
        rollingInterval: RollingInterval.Day,
        retainedFileCountLimit: 30,
        buffered: true,
        flushToDiskInterval: TimeSpan.FromSeconds(5))
    .WriteTo.File(
        path: "logs/security/security-.log",
        rollingInterval: RollingInterval.Day,
        retainedFileCountLimit: 90,
        restrictedToMinimumLevel: Serilog.Events.LogEventLevel.Warning,
        outputTemplate: "{Timestamp:yyyy-MM-dd HH:mm:ss.fff zzz} [{Level:u3}] {Message:lj} {Properties:j}{NewLine}{Exception}")
    .CreateLogger();

builder.Host.UseSerilog();

// Add services to the container.
builder.Services.AddControllers();

// Add CORS
builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowAll",
        policy =>
        {
            policy.AllowAnyOrigin()
                  .AllowAnyMethod()
                  .AllowAnyHeader();
        });
});

// Add Infrastructure services
builder.Services.AddSingleton<IDbConnectionFactory, DbConnectionFactory>();
builder.Services.AddScoped<IUserRepository, UserRepository>();
builder.Services.AddScoped<IRoomRepository, RoomRepository>();
builder.Services.AddScoped<IReservationRepository, ReservationRepository>();

// Add Application services
builder.Services.AddScoped<IJwtTokenService, JwtTokenService>();
builder.Services.AddScoped<IAuthService, AuthService>();
builder.Services.AddScoped<IRoomService, RoomService>();
builder.Services.AddScoped<IReservationService, ReservationService>();

// Add Metrics
builder.Services.AddMetrics();
builder.Services.AddSingleton<IMetricsService, MetricsService>();

// Add Security Services
builder.Services.AddScoped<ISecurityLogService, SecurityLogService>();

// Add Rate Limiting
builder.Services.AddRateLimitingServices();

// Add Data Protection
builder.Services.AddDataProtection();

// Add OpenTelemetry Metrics
builder.Services.AddOpenTelemetry()
    .WithMetrics(builder =>
    {
        builder
            .AddMeter("MRS.Api")
            .AddRuntimeInstrumentation()
            .AddHttpClientInstrumentation()
            .AddAspNetCoreInstrumentation()
            .AddPrometheusExporter();
    });

// Add Health Checks
builder.Services.AddHealthChecks()
    .AddCheck("database", () => 
    {
        try
        {
            var connectionFactory = builder.Services.BuildServiceProvider().GetService<IDbConnectionFactory>();
            using var connection = connectionFactory?.CreateConnection();
            connection?.Open();
            using var command = connection?.CreateCommand();
            command.CommandText = "SELECT COUNT(*) FROM Users";
            var userCount = command.ExecuteScalar();
            return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Healthy($"Database connection successful. Users: {userCount}");
        }
        catch (Exception ex)
        {
            return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Unhealthy($"Database connection failed: {ex.Message}");
        }
    }, tags: new[] { "ready", "database" })
    .AddCheck("memory", () => 
    {
        var allocated = GC.GetTotalMemory(false);
        var availableMemory = GC.GetTotalMemory(true);
        var data = new Dictionary<string, object>()
        {
            { "allocated_bytes", allocated },
            { "available_bytes", availableMemory },
            { "allocated_mb", allocated / 1024 / 1024 },
            { "gen0_collections", GC.CollectionCount(0) },
            { "gen1_collections", GC.CollectionCount(1) },
            { "gen2_collections", GC.CollectionCount(2) }
        };
        
        // 500MB を超えたら警告、1GB を超えたら危険
        var allocatedMB = allocated / 1024 / 1024;
        if (allocatedMB < 500)
        {
            return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Healthy($"Memory usage normal: {allocatedMB}MB", data);
        }
        else if (allocatedMB < 1024)
        {
            return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Degraded($"Memory usage high: {allocatedMB}MB", null, data);
        }
        else
        {
            return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Unhealthy($"Memory usage critical: {allocatedMB}MB", null, data);
        }
    }, tags: new[] { "ready", "memory" })
    .AddCheck("disk_space", () =>
    {
        try
        {
            var drive = new DriveInfo(AppDomain.CurrentDomain.BaseDirectory);
            var freeSpaceGB = drive.AvailableFreeSpace / 1024 / 1024 / 1024;
            var totalSpaceGB = drive.TotalSize / 1024 / 1024 / 1024;
            var usagePercentage = ((double)(totalSpaceGB - freeSpaceGB) / totalSpaceGB) * 100;
            
            var data = new Dictionary<string, object>()
            {
                { "free_space_gb", freeSpaceGB },
                { "total_space_gb", totalSpaceGB },
                { "usage_percentage", usagePercentage }
            };
            
            if (usagePercentage < 80)
            {
                return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Healthy($"Disk usage normal: {usagePercentage:F1}% ({freeSpaceGB}GB free)", data);
            }
            else if (usagePercentage < 90)
            {
                return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Degraded($"Disk usage high: {usagePercentage:F1}% ({freeSpaceGB}GB free)", null, data);
            }
            else
            {
                return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Unhealthy($"Disk usage critical: {usagePercentage:F1}% ({freeSpaceGB}GB free)", null, data);
            }
        }
        catch (Exception ex)
        {
            return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Unhealthy($"Disk space check failed: {ex.Message}");
        }
    }, tags: new[] { "ready", "disk" })
    .AddCheck("application", () =>
    {
        try
        {
            var uptime = DateTime.UtcNow - System.Diagnostics.Process.GetCurrentProcess().StartTime.ToUniversalTime();
            var data = new Dictionary<string, object>()
            {
                { "uptime_minutes", uptime.TotalMinutes },
                { "process_id", Environment.ProcessId },
                { "machine_name", Environment.MachineName },
                { "dotnet_version", Environment.Version.ToString() }
            };
            
            return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Healthy($"Application healthy. Uptime: {uptime.TotalMinutes:F1} minutes", data);
        }
        catch (Exception ex)
        {
            return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Unhealthy($"Application check failed: {ex.Message}");
        }
    }, tags: new[] { "live", "application" });

// Add Health Checks UI
builder.Services.AddHealthChecksUI()
    .AddInMemoryStorage();

// Add Swagger/OpenAPI
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(c =>
{
    c.SwaggerDoc("v1", new OpenApiInfo
    {
        Title = "会議室予約システム (MRS) API",
        Version = "v1",
        Description = "Meeting Room Reservation System RESTful API",
        Contact = new OpenApiContact
        {
            Name = "開発チーム",
            Email = "dev@example.com"
        }
    });

    // JWT認証の設定
    c.AddSecurityDefinition("Bearer", new OpenApiSecurityScheme
    {
        Description = "JWT Authorization header using the Bearer scheme. Example: \"Bearer {token}\"",
        Name = "Authorization",
        In = ParameterLocation.Header,
        Type = SecuritySchemeType.ApiKey,
        Scheme = "Bearer"
    });

    c.AddSecurityRequirement(new OpenApiSecurityRequirement
    {
        {
            new OpenApiSecurityScheme
            {
                Reference = new OpenApiReference
                {
                    Type = ReferenceType.SecurityScheme,
                    Id = "Bearer"
                }
            },
            Array.Empty<string>()
        }
    });
});

var app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI(c =>
    {
        c.SwaggerEndpoint("/swagger/v1/swagger.json", "MRS API v1");
        c.RoutePrefix = string.Empty; // Swagger UIをルートで表示
    });
}

// Middleware pipeline
// 開発環境ではHTTPS強制を無効化
if (!app.Environment.IsDevelopment())
{
    app.UseHttpsRedirection();
}
app.UseCors("AllowAll");

// Add Rate Limiting
app.UseRateLimiter();
app.UseCustomRateLimiting();

// Add Security Logging
app.UseSecurityLogging();

// Add JWT Middleware
app.UseMiddleware<JwtMiddleware>();

app.UseAuthorization();

// Add controller mapping
app.MapControllers();

// Add Health Checks endpoints
app.MapHealthChecks("/health");
app.MapHealthChecks("/health/ready", new Microsoft.AspNetCore.Diagnostics.HealthChecks.HealthCheckOptions()
{
    Predicate = check => check.Tags.Contains("ready")
});
app.MapHealthChecks("/health/live", new Microsoft.AspNetCore.Diagnostics.HealthChecks.HealthCheckOptions()
{
    Predicate = _ => false
});

// Add Health Checks UI
app.MapHealthChecksUI();

// Add Prometheus metrics endpoint
app.MapPrometheusScrapingEndpoint();

app.Run();

// Make Program class accessible for testing
public partial class Program { }