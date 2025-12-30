using System.Diagnostics;
using Grpc.Core;
using Grpc.Core.Interceptors;
using Microsoft.Extensions.Logging;

namespace ProductionManagement.Infrastructure.Grpc.Interceptors;

/// <summary>
/// ロギングインターセプター
/// gRPC リクエスト/レスポンスのログを記録
/// </summary>
public class LoggingInterceptor : Interceptor
{
    private readonly ILogger<LoggingInterceptor> _logger;

    public LoggingInterceptor(ILogger<LoggingInterceptor> logger)
    {
        _logger = logger;
    }

    /// <summary>
    /// Unary RPC のログ記録
    /// </summary>
    public override async Task<TResponse> UnaryServerHandler<TRequest, TResponse>(
        TRequest request,
        ServerCallContext context,
        UnaryServerMethod<TRequest, TResponse> continuation)
    {
        var methodName = context.Method;
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("gRPC Request: {Method}", methodName);

        try
        {
            var response = await continuation(request, context);
            stopwatch.Stop();

            _logger.LogInformation(
                "gRPC Response: {Method} - Duration: {Duration}ms",
                methodName, stopwatch.ElapsedMilliseconds);

            return response;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            _logger.LogError(ex,
                "gRPC Error: {Method} - Duration: {Duration}ms",
                methodName, stopwatch.ElapsedMilliseconds);
            throw;
        }
    }

    /// <summary>
    /// Server Streaming RPC のログ記録
    /// </summary>
    public override async Task ServerStreamingServerHandler<TRequest, TResponse>(
        TRequest request,
        IServerStreamWriter<TResponse> responseStream,
        ServerCallContext context,
        ServerStreamingServerMethod<TRequest, TResponse> continuation)
    {
        var methodName = context.Method;
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("gRPC Streaming Start: {Method}", methodName);

        try
        {
            await continuation(request, responseStream, context);
            stopwatch.Stop();

            _logger.LogInformation(
                "gRPC Streaming End: {Method} - Duration: {Duration}ms",
                methodName, stopwatch.ElapsedMilliseconds);
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            _logger.LogError(ex,
                "gRPC Streaming Error: {Method} - Duration: {Duration}ms",
                methodName, stopwatch.ElapsedMilliseconds);
            throw;
        }
    }

    /// <summary>
    /// Client Streaming RPC のログ記録
    /// </summary>
    public override async Task<TResponse> ClientStreamingServerHandler<TRequest, TResponse>(
        IAsyncStreamReader<TRequest> requestStream,
        ServerCallContext context,
        ClientStreamingServerMethod<TRequest, TResponse> continuation)
    {
        var methodName = context.Method;
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("gRPC Client Streaming Start: {Method}", methodName);

        try
        {
            var response = await continuation(requestStream, context);
            stopwatch.Stop();

            _logger.LogInformation(
                "gRPC Client Streaming End: {Method} - Duration: {Duration}ms",
                methodName, stopwatch.ElapsedMilliseconds);

            return response;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            _logger.LogError(ex,
                "gRPC Client Streaming Error: {Method} - Duration: {Duration}ms",
                methodName, stopwatch.ElapsedMilliseconds);
            throw;
        }
    }

    /// <summary>
    /// Bidirectional Streaming RPC のログ記録
    /// </summary>
    public override async Task DuplexStreamingServerHandler<TRequest, TResponse>(
        IAsyncStreamReader<TRequest> requestStream,
        IServerStreamWriter<TResponse> responseStream,
        ServerCallContext context,
        DuplexStreamingServerMethod<TRequest, TResponse> continuation)
    {
        var methodName = context.Method;
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("gRPC Duplex Streaming Start: {Method}", methodName);

        try
        {
            await continuation(requestStream, responseStream, context);
            stopwatch.Stop();

            _logger.LogInformation(
                "gRPC Duplex Streaming End: {Method} - Duration: {Duration}ms",
                methodName, stopwatch.ElapsedMilliseconds);
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            _logger.LogError(ex,
                "gRPC Duplex Streaming Error: {Method} - Duration: {Duration}ms",
                methodName, stopwatch.ElapsedMilliseconds);
            throw;
        }
    }
}
