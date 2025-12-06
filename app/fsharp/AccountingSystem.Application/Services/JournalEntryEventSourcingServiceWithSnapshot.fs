namespace AccountingSystem.Application.Services

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Events
open AccountingSystem.Domain.Aggregates
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out

/// <summary>
/// 仕訳イベントソーシングサービス（スナップショット最適化版）
/// N イベントごとにスナップショットを作成し、読み込み時はスナップショットから再開
/// </summary>
type JournalEntryEventSourcingServiceWithSnapshot
    (eventStoreRepository: IEventStoreRepository,
     snapshotRepository: ISnapshotRepository,
     ?snapshotInterval: int) =

    let aggregateType = "JournalEntry"
    let snapshotEveryNEvents = defaultArg snapshotInterval 10

    /// <summary>
    /// スナップショット + イベントから Aggregate を復元
    /// </summary>
    let loadAggregateAsync (id: string) : Task<JournalEntryAggregate option> =
        task {
            // スナップショットを取得
            let! snapshot = snapshotRepository.GetSnapshotAsync id aggregateType

            match snapshot with
            | Some snapshotAggregate ->
                // スナップショット以降のイベントを取得
                let! events = eventStoreRepository.GetEventsSinceVersionAsync id snapshotAggregate.Version

                if List.isEmpty events then
                    return Some snapshotAggregate
                else
                    let aggregate = JournalEntryAggregate.replayFrom snapshotAggregate events
                    return Some aggregate
            | None ->
                // スナップショットがない場合は全イベントから復元
                let! events = eventStoreRepository.GetEventsAsync id

                if List.isEmpty events then
                    return None
                else
                    let aggregate = JournalEntryAggregate.replay events
                    return Some aggregate
        }

    /// <summary>
    /// 必要に応じてスナップショットを作成
    /// </summary>
    let createSnapshotIfNeededAsync (aggregate: JournalEntryAggregate) : Task<unit> =
        task {
            // N イベントごとにスナップショットを作成
            if aggregate.Version > 0 && aggregate.Version % snapshotEveryNEvents = 0 then
                let! _ = snapshotRepository.SaveSnapshotAsync aggregate.Id aggregateType aggregate
                ()
        }

    interface IJournalEntryEventSourcingUseCase with

        /// <summary>
        /// 仕訳を作成
        /// </summary>
        member _.CreateJournalEntryAsync
            (id: string)
            (entryDate: DateTime)
            (description: string)
            (lineItems: JournalEntryLineItem list)
            (userId: string)
            : Task<Result<JournalEntryAggregate, string>> =
            task {
                match JournalEntryAggregate.create id entryDate description lineItems userId with
                | Error msg -> return Error msg
                | Ok aggregate ->
                    let! saveResult =
                        eventStoreRepository.SaveAsync
                            id
                            aggregateType
                            aggregate.UncommittedEvents
                            0
                            userId
                            None

                    match saveResult with
                    | Error msg -> return Error msg
                    | Ok () ->
                        let committed = JournalEntryAggregate.markEventsAsCommitted aggregate
                        do! createSnapshotIfNeededAsync committed
                        return Ok committed
            }

        /// <summary>
        /// 仕訳を承認
        /// </summary>
        member _.ApproveJournalEntryAsync
            (id: string)
            (approvedBy: string)
            (approvalComment: string)
            : Task<Result<JournalEntryAggregate, string>> =
            task {
                let! aggregateOpt = loadAggregateAsync id

                match aggregateOpt with
                | None -> return Error "仕訳が見つかりません"
                | Some aggregate ->
                    match JournalEntryAggregate.approve approvedBy approvalComment aggregate with
                    | Error msg -> return Error msg
                    | Ok updatedAggregate ->
                        let! saveResult =
                            eventStoreRepository.SaveAsync
                                id
                                aggregateType
                                updatedAggregate.UncommittedEvents
                                aggregate.Version
                                approvedBy
                                None

                        match saveResult with
                        | Error msg -> return Error msg
                        | Ok () ->
                            let committed = JournalEntryAggregate.markEventsAsCommitted updatedAggregate
                            do! createSnapshotIfNeededAsync committed
                            return Ok committed
            }

        /// <summary>
        /// 仕訳を削除
        /// </summary>
        member _.DeleteJournalEntryAsync
            (id: string)
            (reason: string)
            (userId: string)
            : Task<Result<JournalEntryAggregate, string>> =
            task {
                let! aggregateOpt = loadAggregateAsync id

                match aggregateOpt with
                | None -> return Error "仕訳が見つかりません"
                | Some aggregate ->
                    match JournalEntryAggregate.delete reason userId aggregate with
                    | Error msg -> return Error msg
                    | Ok updatedAggregate ->
                        let! saveResult =
                            eventStoreRepository.SaveAsync
                                id
                                aggregateType
                                updatedAggregate.UncommittedEvents
                                aggregate.Version
                                userId
                                None

                        match saveResult with
                        | Error msg -> return Error msg
                        | Ok () ->
                            let committed = JournalEntryAggregate.markEventsAsCommitted updatedAggregate
                            do! createSnapshotIfNeededAsync committed
                            // 削除時はスナップショットも削除
                            do! snapshotRepository.DeleteSnapshotAsync id aggregateType
                            return Ok committed
            }

        /// <summary>
        /// 仕訳を取得（スナップショット最適化）
        /// </summary>
        member _.GetJournalEntryAsync (id: string) : Task<JournalEntryAggregate option> =
            loadAggregateAsync id

        /// <summary>
        /// 特定時点の仕訳を取得（タイムトラベル）
        /// 注意: タイムトラベルではスナップショットは使用せず、全イベントから復元
        /// </summary>
        member _.GetJournalEntryAtAsync
            (id: string)
            (pointInTime: DateTime)
            : Task<JournalEntryAggregate option> =
            task {
                let! events = eventStoreRepository.GetEventsUntilAsync id pointInTime

                if List.isEmpty events then
                    return None
                else
                    let aggregate = JournalEntryAggregate.replay events
                    return Some aggregate
            }

        /// <summary>
        /// すべての仕訳を取得
        /// </summary>
        member this.GetAllJournalEntriesAsync () : Task<JournalEntryAggregate list> =
            task {
                let! aggregateIds = eventStoreRepository.GetAggregateIdsAsync aggregateType

                let! aggregates =
                    aggregateIds
                    |> List.map (fun id ->
                        task {
                            let! aggregateOpt = loadAggregateAsync id
                            match aggregateOpt with
                            | None -> return None
                            | Some aggregate when aggregate.Deleted -> return None
                            | Some aggregate -> return Some aggregate
                        })
                    |> Task.WhenAll

                return aggregates |> Array.choose id |> Array.toList
            }

    /// <summary>
    /// 手動でスナップショットを作成（バッチ処理用）
    /// </summary>
    member _.CreateSnapshotAsync (id: string) : Task<Result<unit, string>> =
        task {
            let! aggregateOpt = loadAggregateAsync id

            match aggregateOpt with
            | None -> return Error "仕訳が見つかりません"
            | Some aggregate ->
                return! snapshotRepository.SaveSnapshotAsync id aggregateType aggregate
        }

    /// <summary>
    /// スナップショットを削除
    /// </summary>
    member _.DeleteSnapshotAsync (id: string) : Task<unit> =
        snapshotRepository.DeleteSnapshotAsync id aggregateType
