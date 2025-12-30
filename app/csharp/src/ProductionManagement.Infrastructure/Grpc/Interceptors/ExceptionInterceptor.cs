using Grpc.Core;
using Grpc.Core.Interceptors;
using ProductionManagement.Domain.Exceptions;

namespace ProductionManagement.Infrastructure.Grpc.Interceptors;

/// <summary>
/// 例外インターセプター
/// ドメイン例外を gRPC ステータスコードに変換
/// </summary>
public class ExceptionInterceptor : Interceptor
{
    /// <summary>
    /// Unary RPC の例外ハンドリング
    /// </summary>
    public override async Task<TResponse> UnaryServerHandler<TRequest, TResponse>(
        TRequest request,
        ServerCallContext context,
        UnaryServerMethod<TRequest, TResponse> continuation)
    {
        try
        {
            return await continuation(request, context);
        }
        catch (RpcException)
        {
            // 既に RpcException の場合はそのまま再スロー
            throw;
        }
        catch (Exception ex)
        {
            throw MapToRpcException(ex);
        }
    }

    /// <summary>
    /// Server Streaming RPC の例外ハンドリング
    /// </summary>
    public override async Task ServerStreamingServerHandler<TRequest, TResponse>(
        TRequest request,
        IServerStreamWriter<TResponse> responseStream,
        ServerCallContext context,
        ServerStreamingServerMethod<TRequest, TResponse> continuation)
    {
        try
        {
            await continuation(request, responseStream, context);
        }
        catch (RpcException)
        {
            throw;
        }
        catch (Exception ex)
        {
            throw MapToRpcException(ex);
        }
    }

    /// <summary>
    /// Client Streaming RPC の例外ハンドリング
    /// </summary>
    public override async Task<TResponse> ClientStreamingServerHandler<TRequest, TResponse>(
        IAsyncStreamReader<TRequest> requestStream,
        ServerCallContext context,
        ClientStreamingServerMethod<TRequest, TResponse> continuation)
    {
        try
        {
            return await continuation(requestStream, context);
        }
        catch (RpcException)
        {
            throw;
        }
        catch (Exception ex)
        {
            throw MapToRpcException(ex);
        }
    }

    /// <summary>
    /// Bidirectional Streaming RPC の例外ハンドリング
    /// </summary>
    public override async Task DuplexStreamingServerHandler<TRequest, TResponse>(
        IAsyncStreamReader<TRequest> requestStream,
        IServerStreamWriter<TResponse> responseStream,
        ServerCallContext context,
        DuplexStreamingServerMethod<TRequest, TResponse> continuation)
    {
        try
        {
            await continuation(requestStream, responseStream, context);
        }
        catch (RpcException)
        {
            throw;
        }
        catch (Exception ex)
        {
            throw MapToRpcException(ex);
        }
    }

    /// <summary>
    /// 例外を RpcException にマッピング
    /// </summary>
    private static RpcException MapToRpcException(Exception ex)
    {
        return ex switch
        {
            // NotFound 系
            ItemNotFoundException => new RpcException(
                new Status(StatusCode.NotFound, ex.Message)),

            SupplierNotFoundException => new RpcException(
                new Status(StatusCode.NotFound, ex.Message)),

            PurchaseOrderNotFoundException => new RpcException(
                new Status(StatusCode.NotFound, ex.Message)),

            WorkOrderNotFoundException => new RpcException(
                new Status(StatusCode.NotFound, ex.Message)),

            OrderNotFoundException => new RpcException(
                new Status(StatusCode.NotFound, ex.Message)),

            ResourceNotFoundException => new RpcException(
                new Status(StatusCode.NotFound, ex.Message)),

            // AlreadyExists 系
            DuplicateItemException => new RpcException(
                new Status(StatusCode.AlreadyExists, ex.Message)),

            DuplicateSupplierException => new RpcException(
                new Status(StatusCode.AlreadyExists, ex.Message)),

            // FailedPrecondition 系
            InsufficientStockException => new RpcException(
                new Status(StatusCode.FailedPrecondition, ex.Message)),

            InsufficientInventoryException => new RpcException(
                new Status(StatusCode.FailedPrecondition, ex.Message)),

            // InvalidArgument 系（派生クラスを先に）
            ArgumentNullException => new RpcException(
                new Status(StatusCode.InvalidArgument, ex.Message)),

            ArgumentException => new RpcException(
                new Status(StatusCode.InvalidArgument, ex.Message)),

            // ドメイン例外
            DomainException => new RpcException(
                new Status(StatusCode.InvalidArgument, ex.Message)),

            // その他の例外
            _ => new RpcException(
                new Status(StatusCode.Internal, "内部エラーが発生しました"))
        };
    }
}
