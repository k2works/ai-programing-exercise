namespace AccountingSystem.Infrastructure.Persistence.DAO

open System

/// <summary>
/// イベントストアエンティティ（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type EventStoreEntity = {
    event_id: int64
    aggregate_id: string
    aggregate_type: string
    event_type: string
    event_version: int
    event_data: string
    occurred_at: DateTime
    user_id: string
    correlation_id: string
    causation_id: string
    sequence_number: int
}

module EventStoreEntity =
    /// 空のエンティティを作成
    let empty = {
        event_id = 0L
        aggregate_id = ""
        aggregate_type = ""
        event_type = ""
        event_version = 1
        event_data = "{}"
        occurred_at = DateTime.UtcNow
        user_id = ""
        correlation_id = null
        causation_id = null
        sequence_number = 0
    }

    /// 新規イベントエンティティを作成
    let create
        (aggregateId: string)
        (aggregateType: string)
        (eventType: string)
        (eventData: string)
        (userId: string)
        (sequenceNumber: int)
        (correlationId: string option)
        (causationId: string option)
        : EventStoreEntity =
        {
            event_id = 0L
            aggregate_id = aggregateId
            aggregate_type = aggregateType
            event_type = eventType
            event_version = 1
            event_data = eventData
            occurred_at = DateTime.UtcNow
            user_id = userId
            correlation_id = correlationId |> Option.defaultValue null
            causation_id = causationId |> Option.defaultValue null
            sequence_number = sequenceNumber
        }
