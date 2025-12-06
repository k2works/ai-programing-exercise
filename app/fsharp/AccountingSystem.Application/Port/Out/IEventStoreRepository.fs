namespace AccountingSystem.Application.Port.Out

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Events

/// <summary>
/// イベントストアリポジトリインターフェース（Port/Out）
/// </summary>
type IEventStoreRepository =
    /// <summary>
    /// イベントを保存
    /// </summary>
    abstract member SaveAsync:
        aggregateId: string ->
        aggregateType: string ->
        events: JournalEntryEvent list ->
        expectedVersion: int ->
        userId: string ->
        correlationId: string option ->
        Task<Result<unit, string>>

    /// <summary>
    /// Aggregate のすべてのイベントを取得
    /// </summary>
    abstract member GetEventsAsync:
        aggregateId: string ->
        Task<JournalEntryEvent list>

    /// <summary>
    /// 特定時点までのイベントを取得
    /// </summary>
    abstract member GetEventsUntilAsync:
        aggregateId: string ->
        pointInTime: DateTime ->
        Task<JournalEntryEvent list>

    /// <summary>
    /// Aggregate の現在のバージョンを取得
    /// </summary>
    abstract member GetCurrentVersionAsync:
        aggregateId: string ->
        Task<int>

    /// <summary>
    /// 特定タイプのすべての Aggregate ID を取得
    /// </summary>
    abstract member GetAggregateIdsAsync:
        aggregateType: string ->
        Task<string list>

    /// <summary>
    /// 指定バージョン以降のイベントを取得（スナップショット最適化用）
    /// </summary>
    abstract member GetEventsSinceVersionAsync:
        aggregateId: string ->
        sinceVersion: int ->
        Task<JournalEntryEvent list>
