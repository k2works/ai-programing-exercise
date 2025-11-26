using AccountingSystem.Infrastructure.Web.Dtos;
using AccountingSystem.Application.Exceptions;
using Microsoft.AspNetCore.Diagnostics;

namespace AccountingSystem.Infrastructure.Web.Middleware;

/// <summary>
/// グローバル例外ハンドラー
/// </summary>
public class GlobalExceptionHandler : IExceptionHandler
{
    private readonly ILogger<GlobalExceptionHandler> _logger;

    public GlobalExceptionHandler(ILogger<GlobalExceptionHandler> logger)
    {
        _logger = logger;
    }

    public async ValueTask<bool> TryHandleAsync(
        HttpContext httpContext,
        Exception exception,
        CancellationToken cancellationToken)
    {
        var errorResponse = exception switch
        {
            AccountNotFoundException ex => CreateErrorResponse(
                StatusCodes.Status404NotFound,
                "Not Found",
                ex.Message),

            InvalidJournalEntryException ex => CreateErrorResponse(
                StatusCodes.Status400BadRequest,
                "Bad Request",
                ex.Message),

            DuplicateAccountException ex => CreateErrorResponse(
                StatusCodes.Status409Conflict,
                "Conflict",
                ex.Message),

            JournalNotFoundException ex => CreateErrorResponse(
                StatusCodes.Status404NotFound,
                "Not Found",
                ex.Message),

            DuplicateJournalException ex => CreateErrorResponse(
                StatusCodes.Status409Conflict,
                "Conflict",
                ex.Message),

            ArgumentException ex => CreateErrorResponse(
                StatusCodes.Status400BadRequest,
                "Bad Request",
                ex.Message),

            _ => CreateErrorResponse(
                StatusCodes.Status500InternalServerError,
                "Internal Server Error",
                "予期しないエラーが発生しました")
        };

        if (exception is not AccountNotFoundException
            and not InvalidJournalEntryException
            and not DuplicateAccountException
            and not JournalNotFoundException
            and not DuplicateJournalException
            and not ArgumentException)
        {
            _logger.LogError(exception, "Unexpected error occurred");
        }
        else
        {
            _logger.LogWarning("Business exception: {Message}", exception.Message);
        }

        httpContext.Response.StatusCode = errorResponse.Status;
        httpContext.Response.ContentType = "application/json";

        await httpContext.Response.WriteAsJsonAsync(errorResponse, cancellationToken);
        return true;
    }

    private static ErrorResponse CreateErrorResponse(
        int status,
        string error,
        string message)
    {
        return new ErrorResponse
        {
            Status = status,
            Error = error,
            Message = message,
            Timestamp = DateTime.UtcNow
        };
    }
}
