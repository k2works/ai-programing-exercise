namespace AccountingSystem.Application.Services

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Events
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out

/// <summary>
/// イベントディスパッチ結果
/// </summary>
type EventDispatchResult = {
    EventType: string
    AggregateId: string
    HandlersExecuted: int
    SuccessCount: int
    FailureCount: int
    Errors: string list
}

/// <summary>
/// イベントディスパッチャー
/// 登録されたハンドラーにイベントを配信する
/// </summary>
type EventDispatcher(handlers: IJournalEntryEventHandler seq) =

    let handlerList = handlers |> Seq.toList

    /// <summary>
    /// イベントに対応するハンドラーを取得
    /// </summary>
    let getHandlersForEvent (event: JournalEntryEvent) =
        let eventType = JournalEntryEvent.getEventType event
        handlerList
        |> List.filter (fun h -> h.EventTypes |> List.contains eventType)

    /// <summary>
    /// 単一イベントをディスパッチ
    /// </summary>
    member _.DispatchAsync(event: JournalEntryEvent) : Task<EventDispatchResult> =
        task {
            let eventType = JournalEntryEvent.getEventType event
            let aggregateId = JournalEntryEvent.getAggregateId event
            let targetHandlers = getHandlersForEvent event

            let mutable successCount = 0
            let mutable failureCount = 0
            let mutable errors = []

            for handler in targetHandlers do
                let! result = handler.HandleAsync(event)
                match result with
                | Ok () ->
                    successCount <- successCount + 1
                | Error msg ->
                    failureCount <- failureCount + 1
                    errors <- msg :: errors

            return {
                EventType = eventType
                AggregateId = aggregateId
                HandlersExecuted = targetHandlers.Length
                SuccessCount = successCount
                FailureCount = failureCount
                Errors = errors |> List.rev
            }
        }

    /// <summary>
    /// 複数イベントをディスパッチ
    /// </summary>
    member this.DispatchManyAsync(events: JournalEntryEvent list) : Task<EventDispatchResult list> =
        task {
            let results = ResizeArray<EventDispatchResult>()

            for event in events do
                let! result = this.DispatchAsync(event)
                results.Add(result)

            return results |> Seq.toList
        }

    interface IEventPublisher with
        /// <summary>
        /// 単一イベントを発行（ディスパッチ）
        /// </summary>
        member this.PublishAsync(event: JournalEntryEvent) : Task<unit> =
            task {
                let! _ = this.DispatchAsync(event)
                return ()
            }

        /// <summary>
        /// 複数イベントを発行（ディスパッチ）
        /// </summary>
        member this.PublishManyAsync(events: JournalEntryEvent list) : Task<unit> =
            task {
                let! _ = this.DispatchManyAsync(events)
                return ()
            }
