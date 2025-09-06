using Microsoft.AspNetCore.RateLimiting;
using MRS.Api.Services;
using System.Threading.RateLimiting;

namespace MRS.Api.Services;

public static class RateLimitingConfiguration
{
    public static IServiceCollection AddRateLimitingServices(this IServiceCollection services)
    {
        services.AddRateLimiter(options =>
        {
            // グローバルレート制限（すべてのリクエスト）
            options.GlobalLimiter = PartitionedRateLimiter.Create<HttpContext, string>(context =>
                RateLimitPartition.GetFixedWindowLimiter(
                    partitionKey: context.Connection.RemoteIpAddress?.ToString() ?? "unknown",
                    factory: partition => new FixedWindowRateLimiterOptions
                    {
                        AutoReplenishment = true,
                        PermitLimit = 1000, // 1分間に1000リクエスト
                        Window = TimeSpan.FromMinutes(1)
                    }));

            // ログインAPIの制限（より厳しい制限）
            options.AddFixedWindowLimiter("LoginPolicy", options =>
            {
                options.PermitLimit = 5; // 1分間に5回
                options.Window = TimeSpan.FromMinutes(1);
                options.QueueProcessingOrder = QueueProcessingOrder.OldestFirst;
                options.QueueLimit = 2;
            });

            // API全般の制限
            options.AddFixedWindowLimiter("ApiPolicy", options =>
            {
                options.PermitLimit = 100; // 1分間に100リクエスト
                options.Window = TimeSpan.FromMinutes(1);
                options.QueueProcessingOrder = QueueProcessingOrder.OldestFirst;
                options.QueueLimit = 10;
            });

            // 予約作成の制限（スパム防止）
            options.AddFixedWindowLimiter("ReservationPolicy", options =>
            {
                options.PermitLimit = 10; // 1分間に10回の予約作成
                options.Window = TimeSpan.FromMinutes(1);
                options.QueueProcessingOrder = QueueProcessingOrder.OldestFirst;
                options.QueueLimit = 5;
            });

            // スライディングウィンドウ制限（より柔軟な制限）
            options.AddSlidingWindowLimiter("SlidingPolicy", options =>
            {
                options.PermitLimit = 200; // 1分間に200リクエスト
                options.Window = TimeSpan.FromMinutes(1);
                options.SegmentsPerWindow = 6; // 10秒ごとのセグメント
                options.QueueProcessingOrder = QueueProcessingOrder.OldestFirst;
                options.QueueLimit = 20;
            });

            // 同時接続数制限
            options.AddConcurrencyLimiter("ConcurrencyPolicy", options =>
            {
                options.PermitLimit = 50; // 同時50接続まで
                options.QueueProcessingOrder = QueueProcessingOrder.OldestFirst;
                options.QueueLimit = 10;
            });

            // 制限に達した場合の処理
            options.OnRejected = async (context, token) =>
            {
                var securityLogService = context.HttpContext.RequestServices.GetService<ISecurityLogService>();
                var ipAddress = context.HttpContext.Connection.RemoteIpAddress?.ToString();
                var username = context.HttpContext.User?.Identity?.Name;
                var path = context.HttpContext.Request.Path;

                securityLogService?.LogSuspiciousActivity(
                    "RateLimitExceeded",
                    $"レート制限超過: {path} from {ipAddress}",
                    username,
                    ipAddress);

                context.HttpContext.Response.StatusCode = 429; // Too Many Requests
                context.HttpContext.Response.Headers.TryAdd("Retry-After", "60");

                await context.HttpContext.Response.WriteAsync(
                    "Rate limit exceeded. Please try again later.", 
                    cancellationToken: token);
            };

            // IPアドレスベースの制限
            options.AddPolicy("IpPolicy", context =>
            {
                var ipAddress = context.Connection.RemoteIpAddress?.ToString() ?? "unknown";
                
                return RateLimitPartition.GetSlidingWindowLimiter(ipAddress, _ =>
                    new SlidingWindowRateLimiterOptions
                    {
                        PermitLimit = 300, // 5分間に300リクエスト
                        Window = TimeSpan.FromMinutes(5),
                        SegmentsPerWindow = 10, // 30秒ごとのセグメント
                        AutoReplenishment = true
                    });
            });

            // ユーザーベースの制限
            options.AddPolicy("UserPolicy", context =>
            {
                var username = context.User?.Identity?.Name ?? "anonymous";
                
                return RateLimitPartition.GetTokenBucketLimiter(username, _ =>
                    new TokenBucketRateLimiterOptions
                    {
                        TokenLimit = 500, // 最大500トークン
                        QueueProcessingOrder = QueueProcessingOrder.OldestFirst,
                        QueueLimit = 10,
                        ReplenishmentPeriod = TimeSpan.FromMinutes(1), // 1分ごとに補充
                        TokensPerPeriod = 100, // 1分間に100トークン補充
                        AutoReplenishment = true
                    });
            });
        });

        return services;
    }
}

// レート制限属性
[AttributeUsage(AttributeTargets.Method | AttributeTargets.Class)]
public class RateLimitAttribute : Attribute
{
    public string PolicyName { get; }

    public RateLimitAttribute(string policyName)
    {
        PolicyName = policyName;
    }
}

// カスタムレート制限ミドルウェア
public class CustomRateLimitingMiddleware
{
    private readonly RequestDelegate _next;
    private readonly ILogger<CustomRateLimitingMiddleware> _logger;
    private static readonly Dictionary<string, DateTime[]> _requestHistory = new();
    private static readonly object _lock = new object();

    public CustomRateLimitingMiddleware(RequestDelegate next, ILogger<CustomRateLimitingMiddleware> logger)
    {
        _next = next;
        _logger = logger;
    }

    public async Task InvokeAsync(HttpContext context, ISecurityLogService securityLogService)
    {
        var ipAddress = context.Connection.RemoteIpAddress?.ToString() ?? "unknown";
        var path = context.Request.Path.Value?.ToLower();
        
        // 特定のパスに対する特別な制限
        if (await ShouldRateLimit(context, path, ipAddress, securityLogService))
        {
            context.Response.StatusCode = 429;
            context.Response.Headers.TryAdd("Retry-After", "60");
            await context.Response.WriteAsync("Rate limit exceeded for this endpoint");
            return;
        }

        await _next(context);
    }

    private async Task<bool> ShouldRateLimit(HttpContext context, string? path, string ipAddress, ISecurityLogService securityLogService)
    {
        var now = DateTime.UtcNow;
        var key = $"{ipAddress}:{path}";

        lock (_lock)
        {
            if (!_requestHistory.ContainsKey(key))
            {
                _requestHistory[key] = new DateTime[0];
            }

            var requests = _requestHistory[key];
            var recentRequests = requests.Where(r => now - r < TimeSpan.FromMinutes(1)).ToArray();
            _requestHistory[key] = recentRequests.Append(now).ToArray();

            // パス別の制限チェック
            var limit = path switch
            {
                "/api/auth/login" => 5,        // ログインは1分間に5回まで
                "/api/auth/register" => 3,     // 登録は1分間に3回まで
                "/api/reservations" when context.Request.Method == "POST" => 10, // 予約作成は1分間に10回まで
                _ => 100  // その他のAPIは1分間に100回まで
            };

            if (recentRequests.Length >= limit)
            {
                securityLogService.LogSuspiciousActivity(
                    "CustomRateLimitExceeded",
                    $"カスタムレート制限超過: {path} ({recentRequests.Length}/{limit})",
                    context.User?.Identity?.Name,
                    ipAddress);

                return true;
            }

            // 履歴のクリーンアップ（1時間以上前のデータを削除）
            if (_requestHistory.Count > 10000)
            {
                var keysToRemove = _requestHistory
                    .Where(kvp => kvp.Value.All(r => now - r > TimeSpan.FromHours(1)))
                    .Select(kvp => kvp.Key)
                    .ToList();

                foreach (var keyToRemove in keysToRemove)
                {
                    _requestHistory.Remove(keyToRemove);
                }
            }
        }

        return false;
    }
}

public static class CustomRateLimitingExtensions
{
    public static IApplicationBuilder UseCustomRateLimiting(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<CustomRateLimitingMiddleware>();
    }
}