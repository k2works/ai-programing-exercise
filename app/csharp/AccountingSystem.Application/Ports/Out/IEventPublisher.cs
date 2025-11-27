namespace AccountingSystem.Application.Ports.Out;

/// <summary>
/// イベントパブリッシャーインターフェース
/// メッセージブローカーにイベントをパブリッシュするための抽象化
/// </summary>
public interface IEventPublisher
{
    /// <summary>
    /// イベントをパブリッシュ
    /// </summary>
    /// <typeparam name="TEvent">イベントの型</typeparam>
    /// <param name="event">パブリッシュするイベント</param>
    /// <param name="routingKey">ルーティングキー（例: financial.journalentry.created）</param>
    Task PublishAsync<TEvent>(TEvent @event, string routingKey);
}
