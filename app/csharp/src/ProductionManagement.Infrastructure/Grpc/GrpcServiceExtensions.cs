using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Routing;
using Microsoft.Extensions.DependencyInjection;
using ProductionManagement.Infrastructure.Grpc.Interceptors;
using ProductionManagement.Infrastructure.Grpc.Mappers;
using ProductionManagement.Infrastructure.Grpc.Services;

namespace ProductionManagement.Infrastructure.Grpc;

/// <summary>
/// gRPC サービス登録拡張メソッド
/// </summary>
public static class GrpcServiceExtensions
{
    /// <summary>
    /// gRPC サービスを登録する
    /// </summary>
    public static IServiceCollection AddGrpcServices(this IServiceCollection services)
    {
        // gRPC サーバーの登録
        services.AddGrpc(options =>
        {
            options.Interceptors.Add<LoggingInterceptor>();
            options.Interceptors.Add<ExceptionInterceptor>();
        });

        // gRPC リフレクションサービス（開発用）
        services.AddGrpcReflection();

        // Mapper の登録
        services.AddSingleton<ProtoMapper>();

        return services;
    }

    /// <summary>
    /// gRPC エンドポイントをマッピングする
    /// </summary>
    public static IEndpointRouteBuilder MapGrpcServices(this IEndpointRouteBuilder endpoints)
    {
        endpoints.MapGrpcService<ItemGrpcService>();
        endpoints.MapGrpcService<BomGrpcService>();
        endpoints.MapGrpcService<PurchaseOrderGrpcService>();
        endpoints.MapGrpcService<MrpGrpcService>();

        return endpoints;
    }

    /// <summary>
    /// gRPC リフレクションをマッピングする（開発環境用）
    /// </summary>
    public static IEndpointRouteBuilder MapGrpcReflectionIfDevelopment(
        this IEndpointRouteBuilder endpoints,
        bool isDevelopment)
    {
        if (isDevelopment)
        {
            endpoints.MapGrpcReflectionService();
        }

        return endpoints;
    }
}
