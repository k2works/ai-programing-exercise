namespace AccountingSystem.Application.Ports.In;

/// <summary>
/// イベントハンドラーインターフェース
/// 特定のイベントタイプを処理するハンドラーを定義
/// </summary>
/// <typeparam name="TEvent">処理対象のイベント型</typeparam>
public interface IEventBusHandler<TEvent>
{
    /// <summary>
    /// イベントを処理
    /// </summary>
    /// <param name="event">処理対象のイベント</param>
    Task HandleAsync(TEvent @event);
}

/// <summary>
/// イベントコンシューマーインターフェース
/// メッセージブローカーからイベントを購読
/// </summary>
public interface IEventConsumer
{
    /// <summary>
    /// イベントの購読を開始
    /// </summary>
    Task StartAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// イベントの購読を停止
    /// </summary>
    Task StopAsync(CancellationToken cancellationToken = default);
}
