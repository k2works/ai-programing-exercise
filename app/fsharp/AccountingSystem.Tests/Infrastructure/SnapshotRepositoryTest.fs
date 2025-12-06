module AccountingSystem.Tests.Infrastructure.SnapshotRepositoryTest

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open AccountingSystem.Domain.Aggregates
open AccountingSystem.Infrastructure.Persistence.Repositories
open AccountingSystem.Tests.DatabaseTestBase
open Npgsql
open Xunit
open FsUnit.Xunit

/// <summary>
/// スナップショットリポジトリ - Dapper 統合テスト
/// </summary>
type SnapshotRepositoryTest() =
    inherit DatabaseTestBase()

    let aggregateType = "JournalEntry"

    /// テスト用の仕訳明細を作成
    let createTestLineItems () : JournalEntryLineItem list =
        [
            { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 10000m }
            { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 10000m }
        ]

    /// テスト用の Aggregate を作成
    let createTestAggregate (id: string) (version: int) : JournalEntryAggregate =
        {
            Id = id
            EntryDate = DateTime(2024, 1, 15)
            Description = "テスト仕訳"
            LineItems = createTestLineItems ()
            Status = JournalEntryStatus.Draft
            Deleted = false
            Version = version
            UncommittedEvents = []
        }

    /// テストデータをクリーンアップ
    member private this.CleanupTestDataAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use cmd = new NpgsqlCommand("""
                DELETE FROM snapshot_store WHERE aggregate_id LIKE 'SNAP-TEST%';
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    [<Fact>]
    member this.``スナップショットを保存できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = SnapshotRepository(conn)

            let aggregate = createTestAggregate "SNAP-TEST001" 10

            let! result = repository.SaveSnapshotAsync "SNAP-TEST001" aggregateType aggregate

            match result with
            | Ok () -> ()
            | Error msg -> failwith $"Save failed: {msg}"

            let! version = repository.GetSnapshotVersionAsync "SNAP-TEST001" aggregateType
            version |> should equal 10

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``スナップショットを取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = SnapshotRepository(conn)

            let aggregate = createTestAggregate "SNAP-TEST002" 15

            let! _ = repository.SaveSnapshotAsync "SNAP-TEST002" aggregateType aggregate

            let! snapshotOpt = repository.GetSnapshotAsync "SNAP-TEST002" aggregateType

            match snapshotOpt with
            | Some snapshot ->
                snapshot.Id |> should equal "SNAP-TEST002"
                snapshot.Version |> should equal 15
                snapshot.Description |> should equal "テスト仕訳"
                snapshot.LineItems |> should haveLength 2
                snapshot.Status |> should equal JournalEntryStatus.Draft
                snapshot.Deleted |> should equal false
            | None ->
                failwith "Expected snapshot but got None"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``承認済み状態のスナップショットを保存・取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = SnapshotRepository(conn)

            let aggregate = { createTestAggregate "SNAP-TEST003" 20 with Status = JournalEntryStatus.Approved }

            let! _ = repository.SaveSnapshotAsync "SNAP-TEST003" aggregateType aggregate

            let! snapshotOpt = repository.GetSnapshotAsync "SNAP-TEST003" aggregateType

            match snapshotOpt with
            | Some snapshot ->
                snapshot.Status |> should equal JournalEntryStatus.Approved
            | None ->
                failwith "Expected snapshot but got None"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``スナップショットを更新できる（UPSERT）``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = SnapshotRepository(conn)

            // 最初のスナップショットを保存
            let aggregate1 = createTestAggregate "SNAP-TEST004" 10

            let! _ = repository.SaveSnapshotAsync "SNAP-TEST004" aggregateType aggregate1

            // 同じ ID で更新
            let aggregate2 = { createTestAggregate "SNAP-TEST004" 20 with
                                Description = "更新後の仕訳"
                                Status = JournalEntryStatus.Approved }

            let! result = repository.SaveSnapshotAsync "SNAP-TEST004" aggregateType aggregate2

            match result with
            | Ok () -> ()
            | Error msg -> failwith $"Update failed: {msg}"

            let! snapshotOpt = repository.GetSnapshotAsync "SNAP-TEST004" aggregateType

            match snapshotOpt with
            | Some snapshot ->
                snapshot.Version |> should equal 20
                snapshot.Description |> should equal "更新後の仕訳"
                snapshot.Status |> should equal JournalEntryStatus.Approved
            | None ->
                failwith "Expected snapshot but got None"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``スナップショットを削除できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = SnapshotRepository(conn)

            let aggregate = createTestAggregate "SNAP-TEST005" 10

            let! _ = repository.SaveSnapshotAsync "SNAP-TEST005" aggregateType aggregate

            // 削除
            do! repository.DeleteSnapshotAsync "SNAP-TEST005" aggregateType

            // 取得を試みる
            let! snapshotOpt = repository.GetSnapshotAsync "SNAP-TEST005" aggregateType

            snapshotOpt |> should equal None

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``存在しないスナップショットは None を返す``() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = SnapshotRepository(conn)

            let! snapshotOpt = repository.GetSnapshotAsync "NONEXISTENT" aggregateType

            snapshotOpt |> should equal None
        }

    [<Fact>]
    member this.``存在しないスナップショットのバージョンは 0 を返す``() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = SnapshotRepository(conn)

            let! version = repository.GetSnapshotVersionAsync "NONEXISTENT" aggregateType

            version |> should equal 0
        }

    [<Fact>]
    member this.``削除済み状態のスナップショットを保存・取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = SnapshotRepository(conn)

            let aggregate = { createTestAggregate "SNAP-TEST006" 25 with Deleted = true }

            let! _ = repository.SaveSnapshotAsync "SNAP-TEST006" aggregateType aggregate

            let! snapshotOpt = repository.GetSnapshotAsync "SNAP-TEST006" aggregateType

            match snapshotOpt with
            | Some snapshot ->
                snapshot.Deleted |> should equal true
            | None ->
                failwith "Expected snapshot but got None"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``仕訳明細が正しくシリアライズ・デシリアライズされる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = SnapshotRepository(conn)

            let lineItems : JournalEntryLineItem list = [
                { AccountCode = "1100"; DebitCredit = DebitCreditType.Debit; Amount = 5000m }
                { AccountCode = "1200"; DebitCredit = DebitCreditType.Debit; Amount = 3000m }
                { AccountCode = "2100"; DebitCredit = DebitCreditType.Credit; Amount = 8000m }
            ]

            let aggregate = { createTestAggregate "SNAP-TEST007" 5 with LineItems = lineItems }

            let! _ = repository.SaveSnapshotAsync "SNAP-TEST007" aggregateType aggregate

            let! snapshotOpt = repository.GetSnapshotAsync "SNAP-TEST007" aggregateType

            match snapshotOpt with
            | Some snapshot ->
                snapshot.LineItems |> should haveLength 3

                let item1 = snapshot.LineItems.[0]
                item1.AccountCode |> should equal "1100"
                item1.DebitCredit |> should equal DebitCreditType.Debit
                item1.Amount |> should equal 5000m

                let item2 = snapshot.LineItems.[1]
                item2.AccountCode |> should equal "1200"
                item2.DebitCredit |> should equal DebitCreditType.Debit
                item2.Amount |> should equal 3000m

                let item3 = snapshot.LineItems.[2]
                item3.AccountCode |> should equal "2100"
                item3.DebitCredit |> should equal DebitCreditType.Credit
                item3.Amount |> should equal 8000m
            | None ->
                failwith "Expected snapshot but got None"

            do! this.CleanupTestDataAsync()
        }
