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

        try
        {
            await AttachUserToContext(context);
        }
        catch (Exception ex)
        {
            // 予期しないエラーのみログ出力（認証エラーは既にAttachUserToContextで処理済み）
            _logger.LogError(ex, "JWT認証処理で予期しないエラーが発生しました");
            context.Response.StatusCode = 500;
            return;
        }

        await _next(context);
    }

    /// <summary>
    /// JWTトークンからユーザー情報を取得してコンテキストに設定
    /// </summary>
    /// <param name="context">HTTPコンテキスト</param>
    private async Task AttachUserToContext(HttpContext context)
    {
        var token = ExtractTokenFromHeader(context);
        if (string.IsNullOrEmpty(token))
        {
            // トークンがない場合は認証なしで続行
            return;
        }

        try
        {
            var authService = context.RequestServices.GetRequiredService<IAuthService>();
            var userInfo = await authService.ValidateTokenAsync(token, context.RequestAborted);
            
            // ユーザー情報をコンテキストに設定
            context.Items["User"] = userInfo;
        }
        catch (UnauthorizedAccessException)
        {
            // 無効なトークンの場合は、認証なしで続行（認可が必要なエンドポイントで後でチェック）
            return;
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
            "/favicon.ico"
        };

        return !string.IsNullOrEmpty(path) && 
               publicPaths.Any(publicPath => path.StartsWith(publicPath, StringComparison.OrdinalIgnoreCase));
    }
}
