using System.Security.Claims;

namespace MRS.Api.Services;

public interface ISecurityLogService
{
    void LogLoginAttempt(string username, bool success, string? ipAddress = null, string? userAgent = null);
    void LogLogout(string username, string? ipAddress = null);
    void LogUnauthorizedAccess(string resource, string? username = null, string? ipAddress = null);
    void LogPermissionDenied(string resource, string username, string? ipAddress = null);
    void LogDataAccess(string resource, string username, string action, string? ipAddress = null);
    void LogSecurityViolation(string violationType, string details, string? username = null, string? ipAddress = null);
    void LogPasswordChange(string username, string? ipAddress = null);
    void LogAccountLockout(string username, string reason, string? ipAddress = null);
    void LogSuspiciousActivity(string activityType, string details, string? username = null, string? ipAddress = null);
}

public class SecurityLogService : ISecurityLogService
{
    private readonly ILogger<SecurityLogService> _logger;
    private readonly IMetricsService _metricsService;

    public SecurityLogService(ILogger<SecurityLogService> logger, IMetricsService metricsService)
    {
        _logger = logger;
        _metricsService = metricsService;
    }

    public void LogLoginAttempt(string username, bool success, string? ipAddress = null, string? userAgent = null)
    {
        using var scope = _logger.BeginScope(new Dictionary<string, object>
        {
            ["SecurityEvent"] = "LoginAttempt",
            ["Username"] = username,
            ["IpAddress"] = ipAddress ?? "Unknown",
            ["UserAgent"] = userAgent ?? "Unknown",
            ["Timestamp"] = DateTime.UtcNow
        });

        if (success)
        {
            _logger.LogInformation("ユーザー {Username} のログイン成功 from {IpAddress}", username, ipAddress);
            _metricsService.IncrementLoginSuccess();
        }
        else
        {
            _logger.LogWarning("ユーザー {Username} のログイン失敗 from {IpAddress}", username, ipAddress);
            _metricsService.IncrementLoginFailure();
        }
    }

    public void LogLogout(string username, string? ipAddress = null)
    {
        using var scope = _logger.BeginScope(new Dictionary<string, object>
        {
            ["SecurityEvent"] = "Logout",
            ["Username"] = username,
            ["IpAddress"] = ipAddress ?? "Unknown",
            ["Timestamp"] = DateTime.UtcNow
        });

        _logger.LogInformation("ユーザー {Username} のログアウト from {IpAddress}", username, ipAddress);
    }

    public void LogUnauthorizedAccess(string resource, string? username = null, string? ipAddress = null)
    {
        using var scope = _logger.BeginScope(new Dictionary<string, object>
        {
            ["SecurityEvent"] = "UnauthorizedAccess",
            ["Resource"] = resource,
            ["Username"] = username ?? "Anonymous",
            ["IpAddress"] = ipAddress ?? "Unknown",
            ["Timestamp"] = DateTime.UtcNow
        });

        _logger.LogWarning("未認証アクセス試行: Resource={Resource}, User={Username}, IP={IpAddress}", 
            resource, username, ipAddress);
        _metricsService.IncrementUnauthorizedAccess();
    }

    public void LogPermissionDenied(string resource, string username, string? ipAddress = null)
    {
        using var scope = _logger.BeginScope(new Dictionary<string, object>
        {
            ["SecurityEvent"] = "PermissionDenied",
            ["Resource"] = resource,
            ["Username"] = username,
            ["IpAddress"] = ipAddress ?? "Unknown",
            ["Timestamp"] = DateTime.UtcNow
        });

        _logger.LogWarning("アクセス権限不足: Resource={Resource}, User={Username}, IP={IpAddress}", 
            resource, username, ipAddress);
    }

    public void LogDataAccess(string resource, string username, string action, string? ipAddress = null)
    {
        using var scope = _logger.BeginScope(new Dictionary<string, object>
        {
            ["SecurityEvent"] = "DataAccess",
            ["Resource"] = resource,
            ["Username"] = username,
            ["Action"] = action,
            ["IpAddress"] = ipAddress ?? "Unknown",
            ["Timestamp"] = DateTime.UtcNow
        });

        _logger.LogInformation("データアクセス: {Action} on {Resource} by {Username} from {IpAddress}", 
            action, resource, username, ipAddress);
    }

    public void LogSecurityViolation(string violationType, string details, string? username = null, string? ipAddress = null)
    {
        using var scope = _logger.BeginScope(new Dictionary<string, object>
        {
            ["SecurityEvent"] = "SecurityViolation",
            ["ViolationType"] = violationType,
            ["Username"] = username ?? "Anonymous",
            ["IpAddress"] = ipAddress ?? "Unknown",
            ["Timestamp"] = DateTime.UtcNow
        });

        _logger.LogError("セキュリティ違反検出: {ViolationType} - {Details}, User={Username}, IP={IpAddress}", 
            violationType, details, username, ipAddress);
    }

    public void LogPasswordChange(string username, string? ipAddress = null)
    {
        using var scope = _logger.BeginScope(new Dictionary<string, object>
        {
            ["SecurityEvent"] = "PasswordChange",
            ["Username"] = username,
            ["IpAddress"] = ipAddress ?? "Unknown",
            ["Timestamp"] = DateTime.UtcNow
        });

        _logger.LogInformation("パスワード変更: User={Username}, IP={IpAddress}", username, ipAddress);
    }

    public void LogAccountLockout(string username, string reason, string? ipAddress = null)
    {
        using var scope = _logger.BeginScope(new Dictionary<string, object>
        {
            ["SecurityEvent"] = "AccountLockout",
            ["Username"] = username,
            ["Reason"] = reason,
            ["IpAddress"] = ipAddress ?? "Unknown",
            ["Timestamp"] = DateTime.UtcNow
        });

        _logger.LogWarning("アカウントロックアウト: User={Username}, Reason={Reason}, IP={IpAddress}", 
            username, reason, ipAddress);
    }

    public void LogSuspiciousActivity(string activityType, string details, string? username = null, string? ipAddress = null)
    {
        using var scope = _logger.BeginScope(new Dictionary<string, object>
        {
            ["SecurityEvent"] = "SuspiciousActivity",
            ["ActivityType"] = activityType,
            ["Username"] = username ?? "Anonymous",
            ["IpAddress"] = ipAddress ?? "Unknown",
            ["Timestamp"] = DateTime.UtcNow
        });

        _logger.LogWarning("疑わしい活動検出: {ActivityType} - {Details}, User={Username}, IP={IpAddress}", 
            activityType, details, username, ipAddress);
    }
}