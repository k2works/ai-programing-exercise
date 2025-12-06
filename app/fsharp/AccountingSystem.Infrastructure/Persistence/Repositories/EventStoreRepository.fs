namespace AccountingSystem.Infrastructure.Persistence.Repositories

open System
open System.Text.Json
open System.Threading.Tasks
open Npgsql
open Dapper
open AccountingSystem.Domain.Events
open AccountingSystem.Domain.Types
open AccountingSystem.Infrastructure.Persistence.DAO

/// <summary>
/// 同時実行制御例外
/// </summary>
exception ConcurrentModificationException of string

/// <summary>
/// イベントストアリポジトリ実装（Dapper）
/// </summary>
type EventStoreRepository(connection: NpgsqlConnection) =
    let jsonOptions = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

    /// <summary>
    /// JournalEntryLineItem をシリアライズ可能な形式に変換
    /// </summary>
    let serializeLineItem (item: JournalEntryLineItem) =
        {|
            AccountCode = item.AccountCode
            DebitCredit = item.DebitCredit.ToCode()
            Amount = item.Amount
        |}

    /// <summary>
    /// イベントをシリアライズ
    /// </summary>
    let serializeEvent (event: JournalEntryEvent) : string =
        match event with
        | JournalEntryCreated data ->
            let serializable = {|
                JournalEntryId = data.JournalEntryId
                EntryDate = data.EntryDate
                Description = data.Description
                LineItems = data.LineItems |> List.map serializeLineItem
                UserId = data.UserId
                OccurredAt = data.OccurredAt
            |}
            JsonSerializer.Serialize(serializable, jsonOptions)
        | JournalEntryApproved data ->
            JsonSerializer.Serialize(data, jsonOptions)
        | JournalEntryDeleted data ->
            JsonSerializer.Serialize(data, jsonOptions)

    /// <summary>
    /// イベントタイプを取得
    /// </summary>
    let getEventType (event: JournalEntryEvent) : string =
        match event with
        | JournalEntryCreated _ -> "JournalEntryCreated"
        | JournalEntryApproved _ -> "JournalEntryApproved"
        | JournalEntryDeleted _ -> "JournalEntryDeleted"

    /// <summary>
    /// LineItem をデシリアライズ
    /// </summary>
    let deserializeLineItem (json: JsonElement) : JournalEntryLineItem =
        let accountCode = json.GetProperty("AccountCode").GetString()
        let debitCreditCode = json.GetProperty("DebitCredit").GetString()
        let amount = json.GetProperty("Amount").GetDecimal()
        {
            AccountCode = accountCode
            DebitCredit =
                match DebitCreditType.FromCode(debitCreditCode) with
                | Some dc -> dc
                | None -> DebitCreditType.Debit
            Amount = amount
        }

    /// <summary>
    /// イベントをデシリアライズ
    /// </summary>
    let deserializeEvent (entity: EventStoreEntity) : JournalEntryEvent =
        match entity.event_type with
        | "JournalEntryCreated" ->
            let doc = JsonDocument.Parse(entity.event_data)
            let root = doc.RootElement
            let lineItems =
                root.GetProperty("LineItems").EnumerateArray()
                |> Seq.map deserializeLineItem
                |> Seq.toList
            JournalEntryCreated {
                JournalEntryId = root.GetProperty("JournalEntryId").GetString()
                EntryDate = root.GetProperty("EntryDate").GetDateTime()
                Description = root.GetProperty("Description").GetString()
                LineItems = lineItems
                UserId = root.GetProperty("UserId").GetString()
                OccurredAt = root.GetProperty("OccurredAt").GetDateTime()
            }

        | "JournalEntryApproved" ->
            let data = JsonSerializer.Deserialize<JournalEntryApprovedData>(entity.event_data, jsonOptions)
            JournalEntryApproved data

        | "JournalEntryDeleted" ->
            let data = JsonSerializer.Deserialize<JournalEntryDeletedData>(entity.event_data, jsonOptions)
            JournalEntryDeleted data

        | _ ->
            failwith $"Unknown event type: {entity.event_type}"

    /// <summary>
    /// イベントを保存
    /// </summary>
    member _.SaveAsync
        (aggregateId: string)
        (aggregateType: string)
        (events: JournalEntryEvent list)
        (expectedVersion: int)
        (userId: string)
        (correlationId: string option)
        : Task<Result<unit, string>> =
        task {
            try
                // 現在のバージョンを確認（楽観的ロック）
                let checkSql = """
                    SELECT COALESCE(MAX(sequence_number), 0)
                    FROM event_store
                    WHERE aggregate_id = @AggregateId
                """
                let! currentVersion = connection.ExecuteScalarAsync<int>(checkSql, {| AggregateId = aggregateId |})

                if currentVersion <> expectedVersion then
                    return Error $"同時実行エラー: 期待バージョン={expectedVersion}, 現在バージョン={currentVersion}"
                else
                    // イベントを保存
                    let insertSql = """
                        INSERT INTO event_store (
                            aggregate_id, aggregate_type, event_type, event_version,
                            event_data, occurred_at, user_id, correlation_id, causation_id,
                            sequence_number
                        ) VALUES (
                            @aggregate_id, @aggregate_type, @event_type, @event_version,
                            @event_data::jsonb, @occurred_at, @user_id, @correlation_id, @causation_id,
                            @sequence_number
                        )
                    """

                    let mutable sequenceNumber = expectedVersion

                    for event in events do
                        sequenceNumber <- sequenceNumber + 1
                        let entity = EventStoreEntity.create
                                        aggregateId
                                        aggregateType
                                        (getEventType event)
                                        (serializeEvent event)
                                        userId
                                        sequenceNumber
                                        correlationId
                                        None

                        let! _ = connection.ExecuteAsync(insertSql, entity)
                        ()

                    return Ok ()
            with
            | :? PostgresException as ex when ex.SqlState = "23505" ->
                // 一意性制約違反（同時実行）
                return Error "同時実行エラー: 他のプロセスが同時に更新しました"
            | ex ->
                return Error $"イベント保存エラー: {ex.Message}"
        }

    /// <summary>
    /// Aggregate のすべてのイベントを取得
    /// </summary>
    member _.GetEventsAsync (aggregateId: string) : Task<JournalEntryEvent list> =
        task {
            let sql = """
                SELECT event_id, aggregate_id, aggregate_type, event_type, event_version,
                       event_data, occurred_at, user_id, correlation_id, causation_id,
                       sequence_number
                FROM event_store
                WHERE aggregate_id = @AggregateId
                ORDER BY sequence_number
            """

            let! entities = connection.QueryAsync<EventStoreEntity>(sql, {| AggregateId = aggregateId |})
            return entities |> Seq.map deserializeEvent |> Seq.toList
        }

    /// <summary>
    /// 特定時点までのイベントを取得
    /// </summary>
    member _.GetEventsUntilAsync (aggregateId: string) (pointInTime: DateTime) : Task<JournalEntryEvent list> =
        task {
            let sql = """
                SELECT event_id, aggregate_id, aggregate_type, event_type, event_version,
                       event_data, occurred_at, user_id, correlation_id, causation_id,
                       sequence_number
                FROM event_store
                WHERE aggregate_id = @AggregateId
                  AND occurred_at <= @PointInTime
                ORDER BY sequence_number
            """

            let! entities = connection.QueryAsync<EventStoreEntity>(sql, {|
                AggregateId = aggregateId
                PointInTime = pointInTime
            |})
            return entities |> Seq.map deserializeEvent |> Seq.toList
        }

    /// <summary>
    /// Aggregate の現在のバージョンを取得
    /// </summary>
    member _.GetCurrentVersionAsync (aggregateId: string) : Task<int> =
        task {
            let sql = """
                SELECT COALESCE(MAX(sequence_number), 0)
                FROM event_store
                WHERE aggregate_id = @AggregateId
            """

            let! version = connection.ExecuteScalarAsync<int>(sql, {| AggregateId = aggregateId |})
            return version
        }

    /// <summary>
    /// 特定タイプのすべての Aggregate ID を取得
    /// </summary>
    member _.GetAggregateIdsAsync (aggregateType: string) : Task<string list> =
        task {
            let sql = """
                SELECT DISTINCT aggregate_id
                FROM event_store
                WHERE aggregate_type = @AggregateType
                ORDER BY aggregate_id
            """

            let! ids = connection.QueryAsync<string>(sql, {| AggregateType = aggregateType |})
            return ids |> Seq.toList
        }

    /// <summary>
    /// 指定バージョン以降のイベントを取得（スナップショット最適化用）
    /// </summary>
    member _.GetEventsSinceVersionAsync (aggregateId: string) (sinceVersion: int) : Task<JournalEntryEvent list> =
        task {
            let sql = """
                SELECT event_id, aggregate_id, aggregate_type, event_type, event_version,
                       event_data, occurred_at, user_id, correlation_id, causation_id,
                       sequence_number
                FROM event_store
                WHERE aggregate_id = @AggregateId
                  AND sequence_number > @SinceVersion
                ORDER BY sequence_number
            """

            let! entities = connection.QueryAsync<EventStoreEntity>(sql, {|
                AggregateId = aggregateId
                SinceVersion = sinceVersion
            |})
            return entities |> Seq.map deserializeEvent |> Seq.toList
        }
