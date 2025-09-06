using MRS.Api.Middleware;
using MRS.Api.Services;
using MRS.Application.Ports;
using MRS.Application.Services;
using MRS.Infrastructure.Data;
using MRS.Infrastructure.Repositories;
using Microsoft.OpenApi.Models;
using OpenTelemetry.Metrics;

var builder = WebApplication.CreateBuilder(args);

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
            return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Healthy("Database connection successful");
        }
        catch (Exception ex)
        {
            return Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Unhealthy($"Database connection failed: {ex.Message}");
        }
    })
    .AddCheck("memory", () => 
    {
        var allocated = GC.GetTotalMemory(false);
        var data = new Dictionary<string, object>()
        {
            { "allocated", allocated },
            { "gen0", GC.CollectionCount(0) },
            { "gen1", GC.CollectionCount(1) },
            { "gen2", GC.CollectionCount(2) }
        };
        var result = allocated < 100_000_000 
            ? Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Healthy("Memory usage is normal", data)
            : Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult.Degraded("Memory usage is high", null, data);
        return result;
    });

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