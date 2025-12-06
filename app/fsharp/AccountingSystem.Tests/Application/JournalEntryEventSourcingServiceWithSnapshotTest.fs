module AccountingSystem.Tests.Application.JournalEntryEventSourcingServiceWithSnapshotTest

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open AccountingSystem.Domain.Aggregates
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Services
open AccountingSystem.Infrastructure.Persistence.Repositories
open AccountingSystem.Infrastructure.Adapters
open AccountingSystem.Tests.DatabaseTestBase
open Npgsql
open Xunit
open FsUnit.Xunit

/// <summary>
/// JournalEntryEventSourcingServiceWithSnapshot 統合テスト
/// </summary>
type JournalEntryEventSourcingServiceWithSnapshotTest() =
    inherit DatabaseTestBase()

    /// テスト用の仕訳明細を作成
    let createTestLineItems () : JournalEntryLineItem list =
        [
            { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 10000m }
            { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 10000m }
        ]

    /// テストデータをクリーンアップ
    member private this.CleanupTestDataAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use cmd = new NpgsqlCommand("""
                DELETE FROM event_store WHERE aggregate_id LIKE 'SNAP-SVC-TEST%';
                DELETE FROM snapshot_store WHERE aggregate_id LIKE 'SNAP-SVC-TEST%';
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    /// サービスを作成
    member private this.CreateService(?snapshotInterval: int) =
        let eventStoreAdapter = EventStoreRepositoryAdapter(this.ConnectionString)
        let snapshotAdapter = SnapshotRepositoryAdapter(this.ConnectionString)
        match snapshotInterval with
        | Some interval -> JournalEntryEventSourcingServiceWithSnapshot(eventStoreAdapter, snapshotAdapter, interval)
        | None -> JournalEntryEventSourcingServiceWithSnapshot(eventStoreAdapter, snapshotAdapter)

    [<Fact>]
    member this.``仕訳を作成できる``() =
        task {
            do! this.CleanupTestDataAsync()

            let service = this.CreateService() :> IJournalEntryEventSourcingUseCase

            let! result = service.CreateJournalEntryAsync
                            "SNAP-SVC-TEST001"
                            (DateTime(2024, 1, 15))
                            "スナップショットテスト仕訳"
                            (createTestLineItems ())
                            "user1"

            match result with
            | Ok aggregate ->
                aggregate.Id |> should equal "SNAP-SVC-TEST001"
                aggregate.Description |> should equal "スナップショットテスト仕訳"
                aggregate.Version |> should equal 1
                aggregate.Status |> should equal JournalEntryStatus.Draft
            | Error msg ->
                failwith $"Create failed: {msg}"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``仕訳を取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            let service = this.CreateService() :> IJournalEntryEventSourcingUseCase

            let! _ = service.CreateJournalEntryAsync
                        "SNAP-SVC-TEST002"
                        (DateTime(2024, 1, 15))
                        "取得テスト仕訳"
                        (createTestLineItems ())
                        "user1"

            let! aggregateOpt = service.GetJournalEntryAsync "SNAP-SVC-TEST002"

            match aggregateOpt with
            | Some aggregate ->
                aggregate.Id |> should equal "SNAP-SVC-TEST002"
                aggregate.Description |> should equal "取得テスト仕訳"
            | None ->
                failwith "Expected aggregate but got None"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``仕訳を承認できる``() =
        task {
            do! this.CleanupTestDataAsync()

            let service = this.CreateService() :> IJournalEntryEventSourcingUseCase

            let! _ = service.CreateJournalEntryAsync
                        "SNAP-SVC-TEST003"
                        (DateTime(2024, 1, 15))
                        "承認テスト仕訳"
                        (createTestLineItems ())
                        "user1"

            let! result = service.ApproveJournalEntryAsync "SNAP-SVC-TEST003" "manager" "承認します"

            match result with
            | Ok aggregate ->
                aggregate.Status |> should equal JournalEntryStatus.Approved
                aggregate.Version |> should equal 2
            | Error msg ->
                failwith $"Approve failed: {msg}"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``仕訳を削除できる``() =
        task {
            do! this.CleanupTestDataAsync()

            let service = this.CreateService() :> IJournalEntryEventSourcingUseCase

            let! _ = service.CreateJournalEntryAsync
                        "SNAP-SVC-TEST004"
                        (DateTime(2024, 1, 15))
                        "削除テスト仕訳"
                        (createTestLineItems ())
                        "user1"

            let! result = service.DeleteJournalEntryAsync "SNAP-SVC-TEST004" "入力ミス" "user1"

            match result with
            | Ok aggregate ->
                aggregate.Deleted |> should equal true
                aggregate.Version |> should equal 2
            | Error msg ->
                failwith $"Delete failed: {msg}"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``スナップショットが自動的に作成される``() =
        task {
            do! this.CleanupTestDataAsync()

            // スナップショット間隔を 2 に設定
            let serviceWithSnapshot = this.CreateService(2)
            let service = serviceWithSnapshot :> IJournalEntryEventSourcingUseCase

            // 作成（Version = 1）
            let! _ = service.CreateJournalEntryAsync
                        "SNAP-SVC-TEST005"
                        (DateTime(2024, 1, 15))
                        "スナップショット作成テスト"
                        (createTestLineItems ())
                        "user1"

            // 承認（Version = 2）→ スナップショットが作成されるはず
            let! _ = service.ApproveJournalEntryAsync "SNAP-SVC-TEST005" "manager" "承認"

            // スナップショットが作成されたか確認
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let snapshotRepo = SnapshotRepository(conn)

            let! snapshotOpt = snapshotRepo.GetSnapshotAsync "SNAP-SVC-TEST005" "JournalEntry"

            match snapshotOpt with
            | Some snapshot ->
                snapshot.Version |> should equal 2
                snapshot.Status |> should equal JournalEntryStatus.Approved
            | None ->
                failwith "Expected snapshot to be created"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``スナップショットからの復元が正しく動作する``() =
        task {
            do! this.CleanupTestDataAsync()

            // スナップショット間隔を 2 に設定
            let service = this.CreateService(2) :> IJournalEntryEventSourcingUseCase

            // 作成（Version = 1）
            let! _ = service.CreateJournalEntryAsync
                        "SNAP-SVC-TEST006"
                        (DateTime(2024, 1, 15))
                        "復元テスト仕訳"
                        (createTestLineItems ())
                        "user1"

            // 承認（Version = 2）→ スナップショット作成
            let! _ = service.ApproveJournalEntryAsync "SNAP-SVC-TEST006" "manager" "承認"

            // 再度取得（スナップショットから復元されるはず）
            let! aggregateOpt = service.GetJournalEntryAsync "SNAP-SVC-TEST006"

            match aggregateOpt with
            | Some aggregate ->
                aggregate.Version |> should equal 2
                aggregate.Status |> should equal JournalEntryStatus.Approved
                aggregate.Description |> should equal "復元テスト仕訳"
            | None ->
                failwith "Expected aggregate but got None"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``スナップショット後のイベントも正しく適用される``() =
        task {
            do! this.CleanupTestDataAsync()

            // スナップショット間隔を 2 に設定
            let service = this.CreateService(2) :> IJournalEntryEventSourcingUseCase

            // 作成（Version = 1）
            let! _ = service.CreateJournalEntryAsync
                        "SNAP-SVC-TEST007"
                        (DateTime(2024, 1, 15))
                        "追加イベントテスト"
                        (createTestLineItems ())
                        "user1"

            // 承認（Version = 2）→ スナップショット作成
            let! _ = service.ApproveJournalEntryAsync "SNAP-SVC-TEST007" "manager" "承認"

            // 削除（Version = 3）→ スナップショット後のイベント
            let! deleteResult = service.DeleteJournalEntryAsync "SNAP-SVC-TEST007" "取消" "user1"

            match deleteResult with
            | Ok aggregate ->
                aggregate.Version |> should equal 3
                aggregate.Deleted |> should equal true
                aggregate.Status |> should equal JournalEntryStatus.Approved
            | Error msg ->
                failwith $"Delete failed: {msg}"

            // 再度取得してスナップショット + 追加イベントから正しく復元されるか確認
            let! aggregateOpt = service.GetJournalEntryAsync "SNAP-SVC-TEST007"

            match aggregateOpt with
            | Some aggregate ->
                aggregate.Version |> should equal 3
                aggregate.Deleted |> should equal true
            | None ->
                failwith "Expected aggregate but got None"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``削除時にスナップショットも削除される``() =
        task {
            do! this.CleanupTestDataAsync()

            // スナップショット間隔を 2 に設定
            let service = this.CreateService(2) :> IJournalEntryEventSourcingUseCase

            // 作成（Version = 1）
            let! _ = service.CreateJournalEntryAsync
                        "SNAP-SVC-TEST008"
                        (DateTime(2024, 1, 15))
                        "スナップショット削除テスト"
                        (createTestLineItems ())
                        "user1"

            // 承認（Version = 2）→ スナップショット作成
            let! _ = service.ApproveJournalEntryAsync "SNAP-SVC-TEST008" "manager" "承認"

            // スナップショットが作成されたか確認
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let snapshotRepo = SnapshotRepository(conn)

            let! snapshotBefore = snapshotRepo.GetSnapshotAsync "SNAP-SVC-TEST008" "JournalEntry"
            snapshotBefore |> should not' (equal None)

            // 削除
            let! _ = service.DeleteJournalEntryAsync "SNAP-SVC-TEST008" "取消" "user1"

            // スナップショットが削除されたか確認
            let! snapshotAfter = snapshotRepo.GetSnapshotAsync "SNAP-SVC-TEST008" "JournalEntry"
            snapshotAfter |> should equal None

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``手動スナップショット作成が動作する``() =
        task {
            do! this.CleanupTestDataAsync()

            let serviceWithSnapshot = this.CreateService(100) // 高い間隔で自動作成を防ぐ
            let service = serviceWithSnapshot :> IJournalEntryEventSourcingUseCase

            // 作成
            let! _ = service.CreateJournalEntryAsync
                        "SNAP-SVC-TEST009"
                        (DateTime(2024, 1, 15))
                        "手動スナップショットテスト"
                        (createTestLineItems ())
                        "user1"

            // 手動でスナップショットを作成
            let! result = serviceWithSnapshot.CreateSnapshotAsync "SNAP-SVC-TEST009"

            match result with
            | Ok () -> ()
            | Error msg -> failwith $"Manual snapshot creation failed: {msg}"

            // スナップショットが作成されたか確認
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let snapshotRepo = SnapshotRepository(conn)

            let! snapshotOpt = snapshotRepo.GetSnapshotAsync "SNAP-SVC-TEST009" "JournalEntry"

            match snapshotOpt with
            | Some snapshot ->
                snapshot.Version |> should equal 1
            | None ->
                failwith "Expected snapshot to be created manually"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``すべての仕訳を取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            let service = this.CreateService() :> IJournalEntryEventSourcingUseCase

            // 複数の仕訳を作成
            for i in 1..3 do
                let! _ = service.CreateJournalEntryAsync
                            $"SNAP-SVC-TEST010-{i}"
                            (DateTime(2024, 1, 15))
                            $"一覧テスト{i}"
                            (createTestLineItems ())
                            "user1"
                ()

            let! aggregates = service.GetAllJournalEntriesAsync()

            let testAggregates = aggregates |> List.filter (fun a -> a.Id.StartsWith("SNAP-SVC-TEST010"))
            testAggregates |> should haveLength 3

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``削除済みの仕訳は一覧に含まれない``() =
        task {
            do! this.CleanupTestDataAsync()

            let service = this.CreateService() :> IJournalEntryEventSourcingUseCase

            // 仕訳を作成
            let! _ = service.CreateJournalEntryAsync
                        "SNAP-SVC-TEST011"
                        (DateTime(2024, 1, 15))
                        "削除除外テスト"
                        (createTestLineItems ())
                        "user1"

            // 削除
            let! _ = service.DeleteJournalEntryAsync "SNAP-SVC-TEST011" "テスト削除" "user1"

            let! aggregates = service.GetAllJournalEntriesAsync()

            let testAggregates = aggregates |> List.filter (fun a -> a.Id = "SNAP-SVC-TEST011")
            testAggregates |> should haveLength 0

            do! this.CleanupTestDataAsync()
        }
