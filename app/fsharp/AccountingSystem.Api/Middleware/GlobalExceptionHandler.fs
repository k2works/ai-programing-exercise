namespace AccountingSystem.Api.Middleware

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Diagnostics
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open AccountingSystem.Application.Exceptions
open AccountingSystem.Infrastructure.Web.Dtos

/// <summary>
/// グローバル例外ハンドラー
/// アプリケーション全体の例外を処理し、適切な HTTP レスポンスを返す
/// </summary>
type GlobalExceptionHandler(logger: ILogger<GlobalExceptionHandler>) =

    interface IExceptionHandler with
        member _.TryHandleAsync(
            httpContext: HttpContext,
            ex: Exception,
            cancellationToken: CancellationToken) : ValueTask<bool> =
            task {
                let errorResponse =
                    match ex with
                    | :? AccountNotFoundException as e ->
                        logger.LogWarning("Account not found: {Message}", e.Message)
                        ErrorResponse.create
                            StatusCodes.Status404NotFound
                            "Not Found"
                            e.Message

                    | :? DuplicateAccountException as e ->
                        logger.LogWarning("Duplicate account: {Message}", e.Message)
                        ErrorResponse.create
                            StatusCodes.Status409Conflict
                            "Conflict"
                            e.Message

                    | :? JournalNotFoundException as e ->
                        logger.LogWarning("Journal not found: {Message}", e.Message)
                        ErrorResponse.create
                            StatusCodes.Status404NotFound
                            "Not Found"
                            e.Message

                    | :? InvalidJournalEntryException as e ->
                        logger.LogWarning("Invalid journal entry: {Message}", e.Message)
                        ErrorResponse.create
                            StatusCodes.Status400BadRequest
                            "Bad Request"
                            e.Message

                    | :? BusinessRuleViolationException as e ->
                        logger.LogWarning("Business rule violation: {Message}", e.Message)
                        ErrorResponse.create
                            StatusCodes.Status422UnprocessableEntity
                            "Unprocessable Entity"
                            e.Message

                    | :? ArgumentException as e ->
                        logger.LogWarning("Argument exception: {Message}", e.Message)
                        ErrorResponse.create
                            StatusCodes.Status400BadRequest
                            "Bad Request"
                            e.Message

                    | _ ->
                        logger.LogError(ex, "Unexpected error occurred")
                        ErrorResponse.create
                            StatusCodes.Status500InternalServerError
                            "Internal Server Error"
                            "予期しないエラーが発生しました"

                httpContext.Response.StatusCode <- errorResponse.Status
                httpContext.Response.ContentType <- "application/json"

                do! httpContext.Response.WriteAsJsonAsync(errorResponse, cancellationToken)
                return true
            }
            |> ValueTask<bool>
