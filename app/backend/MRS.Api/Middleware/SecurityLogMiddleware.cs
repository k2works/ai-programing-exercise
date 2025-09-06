using MRS.Api.Services;
using System.Security.Claims;
using System.Security;

namespace MRS.Api.Middleware;

public class SecurityLogMiddleware
{
    private readonly RequestDelegate _next;
    private readonly ILogger<SecurityLogMiddleware> _logger;

    public SecurityLogMiddleware(RequestDelegate next, ILogger<SecurityLogMiddleware> logger)
    {
        _next = next;
        _logger = logger;
    }

    public async Task InvokeAsync(HttpContext context, ISecurityLogService securityLogService)
    {
        var ipAddress = GetClientIpAddress(context);
        var userAgent = context.Request.Headers.UserAgent.ToString();
        var username = context.User?.Identity?.Name;

        // リクエストの開始時刻
        var startTime = DateTime.UtcNow;

        try
        {
            await _next(context);

            // レスポンス後の処理
            var duration = DateTime.UtcNow - startTime;

            // セキュリティ関連のエンドポイントのログ記録
            LogSecurityEndpoint(context, securityLogService, ipAddress, userAgent, username);

            // 異常なレスポンス時間の検出
            if (duration.TotalSeconds > 10)
            {
                securityLogService.LogSuspiciousActivity(
                    "SlowResponse", 
                    $"リクエスト処理時間が異常: {duration.TotalSeconds:F2}秒 - {context.Request.Path}",
                    username, 
                    ipAddress);
            }
        }
        catch (UnauthorizedAccessException)
        {
            securityLogService.LogUnauthorizedAccess(context.Request.Path, username, ipAddress);
            throw;
        }
        catch (Exception ex)
        {
            // セキュリティに関連するエラーのログ記録
            if (IsSecurityRelatedException(ex))
            {
                securityLogService.LogSecurityViolation(
                    ex.GetType().Name,
                    ex.Message,
                    username,
                    ipAddress);
            }
            throw;
        }
    }

    private void LogSecurityEndpoint(HttpContext context, ISecurityLogService securityLogService, 
        string? ipAddress, string? userAgent, string? username)
    {
        var path = context.Request.Path.Value?.ToLower();
        var method = context.Request.Method;

        LogAuthEndpoints(context, securityLogService, path, ipAddress, userAgent, username);
        LogApiEndpoints(context, securityLogService, path, method, ipAddress, username);
        DetectAnomalousAccess(context, securityLogService, ipAddress, username);
    }

    private void LogAuthEndpoints(HttpContext context, ISecurityLogService securityLogService,
        string? path, string? ipAddress, string? userAgent, string? username)
    {
        switch (path)
        {
            case "/api/auth/login":
                if (context.Response.StatusCode == 200)
                {
                    securityLogService.LogLoginAttempt(username ?? "Unknown", true, ipAddress, userAgent);
                }
                else if (context.Response.StatusCode == 401)
                {
                    securityLogService.LogLoginAttempt(username ?? "Unknown", false, ipAddress, userAgent);
                }
                break;

            case "/api/auth/logout":
                if (context.Response.StatusCode == 200 && !string.IsNullOrEmpty(username))
                {
                    securityLogService.LogLogout(username, ipAddress);
                }
                break;
        }
    }

    private void LogApiEndpoints(HttpContext context, ISecurityLogService securityLogService,
        string? path, string method, string? ipAddress, string? username)
    {
        switch (path)
        {
            case var reservationPath when reservationPath?.StartsWith("/api/reservations") == true:
                LogReservationEndpoint(context, securityLogService, method, ipAddress, username);
                break;

            case var roomPath when roomPath?.StartsWith("/api/rooms") == true:
                LogRoomEndpoint(context, securityLogService, method, ipAddress, username);
                break;

            case var adminPath when adminPath?.StartsWith("/api/admin") == true:
                LogAdminEndpoint(context, securityLogService, method, ipAddress, username);
                break;
        }
    }

    private void LogReservationEndpoint(HttpContext context, ISecurityLogService securityLogService,
        string method, string? ipAddress, string? username)
    {
        if (context.Response.StatusCode == 200 && !string.IsNullOrEmpty(username))
        {
            var action = method switch
            {
                "GET" => "予約データ閲覧",
                "POST" => "予約作成",
                "PUT" => "予約更新",
                "DELETE" => "予約削除",
                _ => $"予約操作({method})"
            };
            securityLogService.LogDataAccess("Reservations", username, action, ipAddress);
        }
        else if (context.Response.StatusCode == 401)
        {
            securityLogService.LogUnauthorizedAccess($"予約システム({method})", username, ipAddress);
        }
        else if (context.Response.StatusCode == 403)
        {
            securityLogService.LogPermissionDenied($"予約システム({method})", username ?? "Unknown", ipAddress);
        }
    }

    private void LogRoomEndpoint(HttpContext context, ISecurityLogService securityLogService,
        string method, string? ipAddress, string? username)
    {
        if (context.Response.StatusCode == 200 && !string.IsNullOrEmpty(username))
        {
            var action = method switch
            {
                "GET" => "会議室データ閲覧",
                "POST" => "会議室作成",
                "PUT" => "会議室更新",
                "DELETE" => "会議室削除",
                _ => $"会議室操作({method})"
            };
            securityLogService.LogDataAccess("Rooms", username, action, ipAddress);
        }
    }

    private void LogAdminEndpoint(HttpContext context, ISecurityLogService securityLogService,
        string method, string? ipAddress, string? username)
    {
        if (context.Response.StatusCode == 200 && !string.IsNullOrEmpty(username))
        {
            securityLogService.LogDataAccess("AdminPanel", username, $"管理者操作({method})", ipAddress);
        }
        else if (context.Response.StatusCode == 403)
        {
            securityLogService.LogPermissionDenied("AdminPanel", username ?? "Unknown", ipAddress);
        }
    }

    private void DetectAnomalousAccess(HttpContext context, ISecurityLogService securityLogService, 
        string? ipAddress, string? username)
    {
        // SQLインジェクション試行の検出
        var queryString = context.Request.QueryString.Value?.ToLower();
        if (!string.IsNullOrEmpty(queryString) && 
            (queryString.Contains("union select") || 
             queryString.Contains("drop table") || 
             queryString.Contains("delete from") ||
             queryString.Contains("insert into") ||
             queryString.Contains("update set")))
        {
            securityLogService.LogSuspiciousActivity(
                "SQLInjectionAttempt",
                $"SQL インジェクション試行を検出: {queryString}",
                username,
                ipAddress);
        }

        // パストラバーサル試行の検出
        var path = context.Request.Path.Value;
        if (!string.IsNullOrEmpty(path) && 
            (path.Contains("../") || path.Contains("..\\") || path.Contains("%2e%2e")))
        {
            securityLogService.LogSuspiciousActivity(
                "PathTraversalAttempt",
                $"パストラバーサル試行を検出: {path}",
                username,
                ipAddress);
        }

        // 異常なUser-Agentの検出
        var userAgent = context.Request.Headers.UserAgent.ToString().ToLower();
        if (!string.IsNullOrEmpty(userAgent) && 
            (userAgent.Contains("sqlmap") || 
             userAgent.Contains("nmap") || 
             userAgent.Contains("nikto") ||
             userAgent.Contains("dirb")))
        {
            securityLogService.LogSuspiciousActivity(
                "SuspiciousUserAgent",
                $"疑わしい User-Agent を検出: {userAgent}",
                username,
                ipAddress);
        }
    }

    private bool IsSecurityRelatedException(Exception ex)
    {
        return ex is UnauthorizedAccessException ||
               ex is SecurityException ||
               ex is InvalidOperationException ||
               ex.Message.ToLower().Contains("security") ||
               ex.Message.ToLower().Contains("unauthorized") ||
               ex.Message.ToLower().Contains("forbidden");
    }

    private string? GetClientIpAddress(HttpContext context)
    {
        // X-Forwarded-For ヘッダーを優先的に確認
        var xForwardedFor = context.Request.Headers["X-Forwarded-For"].FirstOrDefault();
        if (!string.IsNullOrEmpty(xForwardedFor))
        {
            return xForwardedFor.Split(',')[0].Trim();
        }

        // X-Real-IP ヘッダーを確認
        var xRealIp = context.Request.Headers["X-Real-IP"].FirstOrDefault();
        if (!string.IsNullOrEmpty(xRealIp))
        {
            return xRealIp;
        }

        // RemoteIpAddress を使用
        return context.Connection.RemoteIpAddress?.ToString();
    }
}

public static class SecurityLogMiddlewareExtensions
{
    public static IApplicationBuilder UseSecurityLogging(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<SecurityLogMiddleware>();
    }
}