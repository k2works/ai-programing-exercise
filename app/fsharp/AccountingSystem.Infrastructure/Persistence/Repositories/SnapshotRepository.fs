namespace AccountingSystem.Infrastructure.Persistence.Repositories

open System
open System.Text.Json
open System.Threading.Tasks
open Npgsql
open Dapper
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Aggregates
open AccountingSystem.Infrastructure.Persistence.DAO

/// <summary>
/// スナップショットリポジトリ実装（Dapper）
/// </summary>
type SnapshotRepository(connection: NpgsqlConnection) =
    let jsonOptions = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

    /// <summary>
    /// JournalEntryLineItem をシリアライズ可能な形式に変換
    /// </summary>
    let serializeLineItem (item: AccountingSystem.Domain.Events.JournalEntryLineItem) =
        {|
            AccountCode = item.AccountCode
            DebitCredit = item.DebitCredit.ToCode()
            Amount = item.Amount
        |}

    /// <summary>
    /// Aggregate をシリアライズ
    /// </summary>
    let serializeAggregate (aggregate: JournalEntryAggregate) : string =
        let serializable = {|
            Id = aggregate.Id
            EntryDate = aggregate.EntryDate
            Description = aggregate.Description
            LineItems = aggregate.LineItems |> List.map serializeLineItem
            Status = match aggregate.Status with
                     | JournalEntryStatus.Draft -> "Draft"
                     | JournalEntryStatus.Approved -> "Approved"
            Deleted = aggregate.Deleted
            Version = aggregate.Version
        |}
        JsonSerializer.Serialize(serializable, jsonOptions)

    /// <summary>
    /// LineItem をデシリアライズ
    /// </summary>
    let deserializeLineItem (json: JsonElement) : AccountingSystem.Domain.Events.JournalEntryLineItem =
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
    /// Aggregate をデシリアライズ
    /// </summary>
    let deserializeAggregate (entity: SnapshotStoreEntity) : JournalEntryAggregate =
        let doc = JsonDocument.Parse(entity.snapshot_data)
        let root = doc.RootElement

        let lineItems =
            root.GetProperty("LineItems").EnumerateArray()
            |> Seq.map deserializeLineItem
            |> Seq.toList

        let status =
            match root.GetProperty("Status").GetString() with
            | "Approved" -> JournalEntryStatus.Approved
            | _ -> JournalEntryStatus.Draft

        {
            Id = root.GetProperty("Id").GetString()
            EntryDate = root.GetProperty("EntryDate").GetDateTime()
            Description = root.GetProperty("Description").GetString()
            LineItems = lineItems
            Status = status
            Deleted = root.GetProperty("Deleted").GetBoolean()
            Version = root.GetProperty("Version").GetInt32()
            UncommittedEvents = []
        }

    /// <summary>
    /// スナップショットを保存（UPSERT）
    /// </summary>
    member _.SaveSnapshotAsync
        (aggregateId: string)
        (aggregateType: string)
        (aggregate: JournalEntryAggregate)
        : Task<Result<unit, string>> =
        task {
            try
                let snapshotData = serializeAggregate aggregate

                let sql = """
                    INSERT INTO snapshot_store (
                        aggregate_id, aggregate_type, version, snapshot_data, created_at
                    ) VALUES (
                        @aggregate_id, @aggregate_type, @version, @snapshot_data::jsonb, @created_at
                    )
                    ON CONFLICT (aggregate_id, aggregate_type)
                    DO UPDATE SET
                        version = @version,
                        snapshot_data = @snapshot_data::jsonb,
                        created_at = @created_at
                """

                let entity = SnapshotStoreEntity.create aggregateId aggregateType aggregate.Version snapshotData

                let! _ = connection.ExecuteAsync(sql, entity)
                return Ok ()
            with
            | ex ->
                return Error $"スナップショット保存エラー: {ex.Message}"
        }

    /// <summary>
    /// スナップショットを取得
    /// </summary>
    member _.GetSnapshotAsync
        (aggregateId: string)
        (aggregateType: string)
        : Task<JournalEntryAggregate option> =
        task {
            let sql = """
                SELECT snapshot_id, aggregate_id, aggregate_type, version, snapshot_data, created_at
                FROM snapshot_store
                WHERE aggregate_id = @AggregateId
                  AND aggregate_type = @AggregateType
            """

            let! result = connection.QuerySingleOrDefaultAsync<SnapshotStoreEntity>(sql, {|
                AggregateId = aggregateId
                AggregateType = aggregateType
            |})

            if isNull (box result) || String.IsNullOrEmpty(result.aggregate_id) then
                return None
            else
                return Some (deserializeAggregate result)
        }

    /// <summary>
    /// スナップショットを削除
    /// </summary>
    member _.DeleteSnapshotAsync
        (aggregateId: string)
        (aggregateType: string)
        : Task<unit> =
        task {
            let sql = """
                DELETE FROM snapshot_store
                WHERE aggregate_id = @AggregateId
                  AND aggregate_type = @AggregateType
            """

            let! _ = connection.ExecuteAsync(sql, {|
                AggregateId = aggregateId
                AggregateType = aggregateType
            |})
            return ()
        }

    /// <summary>
    /// スナップショット時点のバージョンを取得
    /// </summary>
    member _.GetSnapshotVersionAsync
        (aggregateId: string)
        (aggregateType: string)
        : Task<int> =
        task {
            let sql = """
                SELECT COALESCE(version, 0)
                FROM snapshot_store
                WHERE aggregate_id = @AggregateId
                  AND aggregate_type = @AggregateType
            """

            let! version = connection.ExecuteScalarAsync<int>(sql, {|
                AggregateId = aggregateId
                AggregateType = aggregateType
            |})
            return version
        }
