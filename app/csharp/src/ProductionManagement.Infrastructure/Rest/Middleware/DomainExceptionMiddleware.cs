using System.Net;
using System.Text.Json;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Diagnostics;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using ProductionManagement.Domain.Exceptions;

namespace ProductionManagement.Infrastructure.Rest.Middleware;

/// <summary>
/// ドメイン例外をHTTPレスポンスに変換するミドルウェア
/// </summary>
public class DomainExceptionMiddleware
{
    private readonly RequestDelegate _next;
    private readonly ILogger<DomainExceptionMiddleware> _logger;

    public DomainExceptionMiddleware(RequestDelegate next, ILogger<DomainExceptionMiddleware> logger)
    {
        _next = next;
        _logger = logger;
    }

    public async Task InvokeAsync(HttpContext context)
    {
        try
        {
            await _next(context);
        }
        catch (DomainException ex)
        {
            _logger.LogWarning(ex, "ドメイン例外が発生しました");
            await HandleDomainExceptionAsync(context, ex);
        }
    }

    private static async Task HandleDomainExceptionAsync(HttpContext context, DomainException exception)
    {
        var (statusCode, errorType, title) = exception switch
        {
            ItemNotFoundException => (HttpStatusCode.NotFound, "item-not-found", "品目が見つかりません"),
            DuplicateItemException => (HttpStatusCode.Conflict, "duplicate-item", "品目コード重複"),
            InsufficientInventoryException => (HttpStatusCode.UnprocessableEntity, "insufficient-inventory", "在庫不足"),
            InsufficientStockException => (HttpStatusCode.UnprocessableEntity, "insufficient-stock", "在庫不足"),
            PurchaseOrderNotFoundException => (HttpStatusCode.NotFound, "purchase-order-not-found", "発注が見つかりません"),
            WorkOrderNotFoundException => (HttpStatusCode.NotFound, "work-order-not-found", "作業指示が見つかりません"),
            SupplierNotFoundException => (HttpStatusCode.NotFound, "supplier-not-found", "取引先が見つかりません"),
            DuplicateSupplierException => (HttpStatusCode.Conflict, "duplicate-supplier", "取引先コード重複"),
            OrderNotFoundException => (HttpStatusCode.NotFound, "order-not-found", "オーダが見つかりません"),
            _ => (HttpStatusCode.InternalServerError, "domain-error", "ドメインエラー")
        };

        var problemDetails = new ProblemDetails
        {
            Status = (int)statusCode,
            Title = title,
            Detail = exception.Message,
            Type = $"https://api.example.com/errors/{errorType}",
            Instance = context.Request.Path
        };

        problemDetails.Extensions["timestamp"] = DateTime.UtcNow.ToString("O");

        var options = new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
        };

        await Results.Json(
            problemDetails,
            options,
            contentType: "application/problem+json",
            statusCode: (int)statusCode
        ).ExecuteAsync(context);
    }
}

/// <summary>
/// グローバル例外ハンドラの設定
/// </summary>
public static class GlobalExceptionHandler
{
    /// <summary>
    /// 予期しない例外を処理する汎用エラーハンドラを設定
    /// </summary>
    public static void ConfigureExceptionHandler(this WebApplication app)
    {
        app.UseExceptionHandler(errorApp =>
        {
            errorApp.Run(async context =>
            {
                var exceptionHandler = context.Features.Get<IExceptionHandlerFeature>();
                var exception = exceptionHandler?.Error;

                if (exception is null)
                {
                    return;
                }

                var logger = context.RequestServices.GetService<ILogger<DomainExceptionMiddleware>>();
                logger?.LogError(exception, "予期しないエラーが発生しました");

                var problemDetails = new ProblemDetails
                {
                    Status = StatusCodes.Status500InternalServerError,
                    Title = "内部エラー",
                    Detail = app.Environment.IsDevelopment()
                        ? exception.Message
                        : "予期しないエラーが発生しました",
                    Type = "https://api.example.com/errors/internal-error",
                    Instance = context.Request.Path
                };

                problemDetails.Extensions["timestamp"] = DateTime.UtcNow.ToString("O");

                if (app.Environment.IsDevelopment())
                {
                    problemDetails.Extensions["stackTrace"] = exception.StackTrace;
                }

                var options = new JsonSerializerOptions
                {
                    PropertyNamingPolicy = JsonNamingPolicy.CamelCase
                };

                await Results.Json(
                    problemDetails,
                    options,
                    contentType: "application/problem+json",
                    statusCode: StatusCodes.Status500InternalServerError
                ).ExecuteAsync(context);
            });
        });
    }
}
