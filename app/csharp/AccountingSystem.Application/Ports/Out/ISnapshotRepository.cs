namespace AccountingSystem.Application.Ports.Out;

using AccountingSystem.Domain.Aggregates;

/// <summary>
/// スナップショットリポジトリインターフェース
/// </summary>
public interface ISnapshotRepository
{
    /// <summary>
    /// スナップショットを保存
    /// </summary>
    /// <param name="aggregateId">集約ID</param>
    /// <param name="version">バージョン</param>
    /// <param name="aggregate">集約の状態</param>
    Task SaveSnapshotAsync(string aggregateId, int version, JournalEntryAggregate aggregate);

    /// <summary>
    /// 最新のスナップショットを取得
    /// </summary>
    /// <param name="aggregateId">集約ID</param>
    Task<Snapshot?> GetLatestSnapshotAsync(string aggregateId);
}

/// <summary>
/// スナップショットレコード
/// </summary>
public record Snapshot(
    string AggregateId,
    int Version,
    JournalEntryAggregate Aggregate
);
