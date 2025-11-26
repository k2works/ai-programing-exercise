namespace AccountingSystem.Application.Ports.Out;

using AccountingSystem.Domain.Events;

/// <summary>
/// イベントストアリポジトリインターフェース
/// </summary>
public interface IEventStoreRepository
{
    /// <summary>
    /// イベントを保存
    /// </summary>
    /// <param name="aggregateId">集約ID</param>
    /// <param name="events">保存するイベント</param>
    /// <param name="expectedVersion">期待するバージョン（楽観的ロック用）</param>
    Task SaveAsync(string aggregateId, IReadOnlyList<IEventSourcedDomainEvent> events, int expectedVersion);

    /// <summary>
    /// Aggregate のすべてのイベントを取得
    /// </summary>
    /// <param name="aggregateId">集約ID</param>
    Task<IReadOnlyList<IEventSourcedDomainEvent>> GetEventsAsync(string aggregateId);

    /// <summary>
    /// 特定時点までのイベントを取得
    /// </summary>
    /// <param name="aggregateId">集約ID</param>
    /// <param name="pointInTime">時点</param>
    Task<IReadOnlyList<IEventSourcedDomainEvent>> GetEventsUntilAsync(string aggregateId, DateTime pointInTime);

    /// <summary>
    /// Aggregate の現在のバージョンを取得
    /// </summary>
    /// <param name="aggregateId">集約ID</param>
    Task<int> GetCurrentVersionAsync(string aggregateId);
}
