using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using MRS.Application.Ports;
using MRS.Application.DTOs.Auth;

namespace MRS.Api.Middleware;

/// <summary>
/// JWT認証ミドルウェア
/// </summary>
public class JwtMiddleware
{
    private readonly RequestDelegate _next;
    private readonly ILogger<JwtMiddleware> _logger;

    // ログメッセージのパフォーマンス向上
    private static readonly Action<ILogger, Exception?> LogAuthenticationError =
        LoggerMessage.Define(
            LogLevel.Warning,
            new EventId(1, "AuthenticationError"),
            "JWT認証処理でエラーが発生しました");

    /// <summary>
    /// JwtMiddlewareのコンストラクタ
    /// </summary>
    /// <param name="next">次のミドルウェア</param>
    /// <param name="logger">ロガー</param>
    public JwtMiddleware(RequestDelegate next, ILogger<JwtMiddleware> logger)
    {
        _next = next ?? throw new ArgumentNullException(nameof(next));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <summary>
    /// リクエスト処理
    /// </summary>
    /// <param name="context">HTTPコンテキスト</param>
    public async Task InvokeAsync(HttpContext context)
    {
        // 認証不要のパスをスキップ
        if (ShouldSkipAuthentication(context))
        {
            await _next(context);
            return;
        }

        var shouldContinue = await AttachUserToContext(context);
        if (!shouldContinue)
        {
            return;
        }

        await _next(context);
    }

    /// <summary>
    /// JWTトークンからユーザー情報を取得してコンテキストに設定
    /// </summary>
    /// <param name="context">HTTPコンテキスト</param>
    /// <returns>処理を続行するかどうか</returns>
    private async Task<bool> AttachUserToContext(HttpContext context)
    {
        var token = ExtractTokenFromHeader(context);
        var path = context.Request.Path.Value;
        
        if (string.IsNullOrEmpty(token))
        {
            _logger.LogInformation("JWT トークンが見つかりません。Path: {Path}", path);
            
            // 保護されたエンドポイントの場合は401を返す
            if (IsProtectedEndpoint(context))
            {
                _logger.LogWarning("保護されたエンドポイント {Path} にトークンなしでアクセス", path);
                context.Response.StatusCode = 401;
                return false;
            }
            // パブリックエンドポイントの場合は認証なしで続行
            _logger.LogInformation("パブリックエンドポイント {Path} は認証なしで続行", path);
            return true;
        }

        _logger.LogInformation("JWT トークンを検証中。Path: {Path}", path);
        
        try
        {
            var authService = context.RequestServices.GetRequiredService<IAuthService>();
            var userInfo = await authService.ValidateTokenAsync(token, context.RequestAborted);
            
            // ユーザー情報をコンテキストに設定
            context.Items["User"] = userInfo;
            _logger.LogInformation("JWT 認証成功。User: {UserId}, Path: {Path}", userInfo?.UserId, path);
            return true;
        }
        catch (UnauthorizedAccessException ex)
        {
            // 無効なトークンの場合は401を返す
            _logger.LogWarning(ex, "無効なJWTトークン。Path: {Path}", path);
            context.Response.StatusCode = 401;
            return false;
        }
        catch (Exception ex)
        {
            // その他の予期しないエラーの場合は401を返す
            _logger.LogError(ex, "JWT認証処理で予期しないエラーが発生しました。Path: {Path}", path);
            context.Response.StatusCode = 401;
            return false;
        }
    }

    /// <summary>
    /// Authorizationヘッダーからトークンを抽出
    /// </summary>
    /// <param name="context">HTTPコンテキスト</param>
    /// <returns>JWTトークン（Bearer プレフィックス除去済み）</returns>
    private static string? ExtractTokenFromHeader(HttpContext context)
    {
        var authHeader = context.Request.Headers.Authorization.FirstOrDefault();
        if (string.IsNullOrEmpty(authHeader))
        {
            return null;
        }

        // "Bearer " プレフィックスをチェック
        const string bearerPrefix = "Bearer ";
        if (!authHeader.StartsWith(bearerPrefix, StringComparison.OrdinalIgnoreCase))
        {
            return null;
        }

        return authHeader[bearerPrefix.Length..];
    }

    /// <summary>
    /// 認証をスキップするパスかどうかを判定
    /// </summary>
    /// <param name="context">HTTPコンテキスト</param>
    /// <returns>認証をスキップする場合true</returns>
    private static bool ShouldSkipAuthentication(HttpContext context)
    {
        var path = context.Request.Path.Value?.ToLowerInvariant();
        
        // 認証不要のパス
        var publicPaths = new[]
        {
            "/swagger",
            "/swagger/index.html",
            "/swagger/v1/swagger.json",
            "/api/auth/login",
            "/_framework",
            "/favicon.ico",
            "/health",
            "/healthchecks",
            "/metrics"
        };

        return !string.IsNullOrEmpty(path) && 
               publicPaths.Any(publicPath => path.StartsWith(publicPath, StringComparison.OrdinalIgnoreCase));
    }

    /// <summary>
    /// 保護されたエンドポイントかどうかを判定
    /// </summary>
    /// <param name="context">HTTPコンテキスト</param>
    /// <returns>保護されたエンドポイントの場合true</returns>
    private static bool IsProtectedEndpoint(HttpContext context)
    {
        var path = context.Request.Path.Value?.ToLowerInvariant();
        
        // 保護されたパス（認証が必要）
        var protectedPaths = new[]
        {
            "/api/rooms",
            "/api/reservations",
            "/api/users",
            "/api/backup"
        };

        return !string.IsNullOrEmpty(path) && 
               protectedPaths.Any(protectedPath => path.StartsWith(protectedPath, StringComparison.OrdinalIgnoreCase));
    }
}
