namespace AccountingSystem.Application.Port.Out

open System.Threading.Tasks
open AccountingSystem.Domain.Aggregates

/// <summary>
/// スナップショットリポジトリインターフェース（Output Port）
/// イベントソーシングの最適化のためのスナップショット機能
/// </summary>
type ISnapshotRepository =
    /// <summary>
    /// スナップショットを保存（UPSERT）
    /// </summary>
    /// <param name="aggregateId">Aggregate ID</param>
    /// <param name="aggregateType">Aggregate タイプ</param>
    /// <param name="aggregate">スナップショット対象の Aggregate</param>
    /// <returns>成功時は Ok、失敗時は Error</returns>
    abstract member SaveSnapshotAsync:
        aggregateId: string ->
        aggregateType: string ->
        aggregate: JournalEntryAggregate ->
        Task<Result<unit, string>>

    /// <summary>
    /// スナップショットを取得
    /// </summary>
    /// <param name="aggregateId">Aggregate ID</param>
    /// <param name="aggregateType">Aggregate タイプ</param>
    /// <returns>スナップショットが存在する場合は Some、なければ None</returns>
    abstract member GetSnapshotAsync:
        aggregateId: string ->
        aggregateType: string ->
        Task<JournalEntryAggregate option>

    /// <summary>
    /// スナップショットを削除
    /// </summary>
    /// <param name="aggregateId">Aggregate ID</param>
    /// <param name="aggregateType">Aggregate タイプ</param>
    abstract member DeleteSnapshotAsync:
        aggregateId: string ->
        aggregateType: string ->
        Task<unit>

    /// <summary>
    /// スナップショット時点のバージョンを取得
    /// </summary>
    /// <param name="aggregateId">Aggregate ID</param>
    /// <param name="aggregateType">Aggregate タイプ</param>
    /// <returns>スナップショットのバージョン（なければ 0）</returns>
    abstract member GetSnapshotVersionAsync:
        aggregateId: string ->
        aggregateType: string ->
        Task<int>
