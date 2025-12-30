using System.Net.Http;
using Grpc.Net.Client;
using Microsoft.Extensions.Configuration;

namespace ProductionManagement.WPF.Services;

/// <summary>
/// gRPC チャネルファクトリー
/// </summary>
public class GrpcChannelFactory : IGrpcChannelFactory
{
    private readonly GrpcChannel _channel;
    private bool _disposed;

    public GrpcChannelFactory(IConfiguration configuration)
    {
        var serverAddress = configuration.GetValue<string>("GrpcServer:Address")
            ?? "http://localhost:5000";

        var handler = new SocketsHttpHandler
        {
            PooledConnectionIdleTimeout = Timeout.InfiniteTimeSpan,
            KeepAlivePingDelay = TimeSpan.FromSeconds(60),
            KeepAlivePingTimeout = TimeSpan.FromSeconds(30),
            EnableMultipleHttp2Connections = true
        };

        _channel = GrpcChannel.ForAddress(serverAddress, new GrpcChannelOptions
        {
            HttpHandler = handler
        });
    }

    /// <summary>
    /// gRPC チャネル
    /// </summary>
    public GrpcChannel Channel => _channel;

    /// <summary>
    /// 接続テストを実行
    /// </summary>
    public async Task<bool> TestConnectionAsync()
    {
        try
        {
            await _channel.ConnectAsync();
            return true;
        }
        catch
        {
            return false;
        }
    }

    public void Dispose()
    {
        if (!_disposed)
        {
            _channel.Dispose();
            _disposed = true;
        }
    }
}
