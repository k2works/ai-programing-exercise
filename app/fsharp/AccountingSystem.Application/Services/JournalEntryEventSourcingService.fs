namespace AccountingSystem.Application.Services

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Events
open AccountingSystem.Domain.Aggregates
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out

/// <summary>
/// 仕訳イベントソーシングサービス
/// </summary>
type JournalEntryEventSourcingService(eventStoreRepository: IEventStoreRepository) =

    let aggregateType = "JournalEntry"

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
                // Aggregate コマンドを実行
                match JournalEntryAggregate.create id entryDate description lineItems userId with
                | Error msg -> return Error msg
                | Ok aggregate ->
                    // イベントを保存
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
                        return Ok (JournalEntryAggregate.markEventsAsCommitted aggregate)
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
                // 現在の状態を復元
                let! events = eventStoreRepository.GetEventsAsync id

                if List.isEmpty events then
                    return Error "仕訳が見つかりません"
                else
                    let aggregate = JournalEntryAggregate.replay events

                    // Aggregate コマンドを実行
                    match JournalEntryAggregate.approve approvedBy approvalComment aggregate with
                    | Error msg -> return Error msg
                    | Ok updatedAggregate ->
                        // イベントを保存
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
                            return Ok (JournalEntryAggregate.markEventsAsCommitted updatedAggregate)
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
                // 現在の状態を復元
                let! events = eventStoreRepository.GetEventsAsync id

                if List.isEmpty events then
                    return Error "仕訳が見つかりません"
                else
                    let aggregate = JournalEntryAggregate.replay events

                    // Aggregate コマンドを実行
                    match JournalEntryAggregate.delete reason userId aggregate with
                    | Error msg -> return Error msg
                    | Ok updatedAggregate ->
                        // イベントを保存
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
                            return Ok (JournalEntryAggregate.markEventsAsCommitted updatedAggregate)
            }

        /// <summary>
        /// 仕訳を取得（イベント再生）
        /// </summary>
        member _.GetJournalEntryAsync (id: string) : Task<JournalEntryAggregate option> =
            task {
                let! events = eventStoreRepository.GetEventsAsync id

                if List.isEmpty events then
                    return None
                else
                    let aggregate = JournalEntryAggregate.replay events
                    return Some aggregate
            }

        /// <summary>
        /// 特定時点の仕訳を取得（タイムトラベル）
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
                            let! events = eventStoreRepository.GetEventsAsync id
                            if List.isEmpty events then
                                return None
                            else
                                let aggregate = JournalEntryAggregate.replay events
                                // 削除済みは除外
                                if aggregate.Deleted then
                                    return None
                                else
                                    return Some aggregate
                        })
                    |> Task.WhenAll

                return aggregates |> Array.choose id |> Array.toList
            }
