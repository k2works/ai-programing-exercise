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
        try
        {
            await AttachUserToContext(context);
        }
        catch (UnauthorizedAccessException ex)
        {
            LogAuthenticationError(_logger, ex);
            context.Response.StatusCode = 401;
            return;
        }
        catch (Exception ex)
        {
            LogAuthenticationError(_logger, ex);
            context.Response.StatusCode = 401;
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

        var authService = context.RequestServices.GetRequiredService<IAuthService>();
        var userInfo = await authService.ValidateTokenAsync(token, context.RequestAborted);
        
        // ユーザー情報をコンテキストに設定
        context.Items["User"] = userInfo;
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
}
