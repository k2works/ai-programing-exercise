using Grpc.Net.Client;

namespace ProductionManagement.WPF.Services;

/// <summary>
/// gRPC チャネルファクトリーインターフェース
/// </summary>
public interface IGrpcChannelFactory : IDisposable
{
    /// <summary>
    /// gRPC チャネルを取得
    /// </summary>
    GrpcChannel Channel { get; }

    /// <summary>
    /// 接続テストを実行
    /// </summary>
    Task<bool> TestConnectionAsync();
}
