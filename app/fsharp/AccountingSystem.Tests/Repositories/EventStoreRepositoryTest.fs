module AccountingSystem.Tests.Repositories.EventStoreRepositoryTest

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open AccountingSystem.Infrastructure.Persistence.Repositories
open AccountingSystem.Tests.DatabaseTestBase
open Npgsql
open Xunit
open FsUnit.Xunit

/// <summary>
/// イベントストアリポジトリ - Dapper 統合テスト
/// </summary>
type EventStoreRepositoryTest() =
    inherit DatabaseTestBase()

    /// テスト用の仕訳明細を作成
    let createTestLineItems () =
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
                DELETE FROM event_store WHERE aggregate_id LIKE 'TEST%';
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    [<Fact>]
    member this.``イベントを保存できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = EventStoreRepository(conn)

            let events : JournalEntryEvent list = [
                JournalEntryCreated {
                    JournalEntryId = "TEST001"
                    EntryDate = DateTime(2024, 1, 15)
                    Description = "テスト仕訳"
                    LineItems = createTestLineItems ()
                    UserId = "user1"
                    OccurredAt = DateTime.UtcNow
                }
            ]

            let! result = repository.SaveAsync "TEST001" "JournalEntry" events 0 "user1" None

            match result with
            | Ok () -> ()
            | Error msg -> failwith $"Save failed: {msg}"

            let! version = repository.GetCurrentVersionAsync "TEST001"
            version |> should equal 1

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``複数イベントを保存できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = EventStoreRepository(conn)

            let createEvents : JournalEntryEvent list = [
                JournalEntryCreated {
                    JournalEntryId = "TEST002"
                    EntryDate = DateTime(2024, 1, 15)
                    Description = "テスト仕訳2"
                    LineItems = createTestLineItems ()
                    UserId = "user1"
                    OccurredAt = DateTime.UtcNow
                }
            ]

            let! _ = repository.SaveAsync "TEST002" "JournalEntry" createEvents 0 "user1" None

            let approveEvents : JournalEntryEvent list = [
                JournalEntryApproved {
                    JournalEntryId = "TEST002"
                    ApprovedBy = "manager"
                    ApprovalComment = "承認"
                    OccurredAt = DateTime.UtcNow
                    UserId = "manager"
                }
            ]

            let! result = repository.SaveAsync "TEST002" "JournalEntry" approveEvents 1 "manager" None

            match result with
            | Ok () -> ()
            | Error msg -> failwith $"Save failed: {msg}"

            let! version = repository.GetCurrentVersionAsync "TEST002"
            version |> should equal 2

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``イベントを取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = EventStoreRepository(conn)

            let createEvents : JournalEntryEvent list = [
                JournalEntryCreated {
                    JournalEntryId = "TEST003"
                    EntryDate = DateTime(2024, 1, 15)
                    Description = "取得テスト"
                    LineItems = createTestLineItems ()
                    UserId = "user1"
                    OccurredAt = DateTime(2024, 1, 15, 10, 0, 0)
                }
            ]

            let! _ = repository.SaveAsync "TEST003" "JournalEntry" createEvents 0 "user1" None

            let! events = repository.GetEventsAsync "TEST003"

            events |> should haveLength 1

            match events.Head with
            | JournalEntryCreated data ->
                data.JournalEntryId |> should equal "TEST003"
                data.Description |> should equal "取得テスト"
                data.LineItems |> should haveLength 2
            | _ ->
                failwith "Expected JournalEntryCreated event"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``楽観的ロックが動作する``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = EventStoreRepository(conn)

            let createEvents : JournalEntryEvent list = [
                JournalEntryCreated {
                    JournalEntryId = "TEST004"
                    EntryDate = DateTime(2024, 1, 15)
                    Description = "楽観的ロックテスト"
                    LineItems = createTestLineItems ()
                    UserId = "user1"
                    OccurredAt = DateTime.UtcNow
                }
            ]

            let! _ = repository.SaveAsync "TEST004" "JournalEntry" createEvents 0 "user1" None

            // 古いバージョンでイベントを追加しようとする
            let approveEvents : JournalEntryEvent list = [
                JournalEntryApproved {
                    JournalEntryId = "TEST004"
                    ApprovedBy = "manager"
                    ApprovalComment = "承認"
                    OccurredAt = DateTime.UtcNow
                    UserId = "manager"
                }
            ]

            let! result = repository.SaveAsync "TEST004" "JournalEntry" approveEvents 0 "manager" None

            match result with
            | Error msg ->
                Assert.Contains("同時実行エラー", msg)
            | Ok () ->
                failwith "Expected Error but got Ok"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``Aggregate ID 一覧を取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = EventStoreRepository(conn)

            // 複数の Aggregate を作成
            for i in 1..3 do
                let events : JournalEntryEvent list = [
                    JournalEntryCreated {
                        JournalEntryId = $"TEST005-{i}"
                        EntryDate = DateTime(2024, 1, 15)
                        Description = $"一覧テスト{i}"
                        LineItems = createTestLineItems ()
                        UserId = "user1"
                        OccurredAt = DateTime.UtcNow
                    }
                ]
                let! _ = repository.SaveAsync $"TEST005-{i}" "JournalEntry" events 0 "user1" None
                ()

            let! aggregateIds = repository.GetAggregateIdsAsync "JournalEntry"

            aggregateIds |> should contain "TEST005-1"
            aggregateIds |> should contain "TEST005-2"
            aggregateIds |> should contain "TEST005-3"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``存在しない Aggregate のイベントは空リストを返す``() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = EventStoreRepository(conn)

            let! events = repository.GetEventsAsync "NONEXISTENT"

            events |> should haveLength 0
        }

    [<Fact>]
    member this.``特定時点までのイベントを取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = EventStoreRepository(conn)

            // 作成イベントを保存
            let createEvents : JournalEntryEvent list = [
                JournalEntryCreated {
                    JournalEntryId = "TEST006"
                    EntryDate = DateTime(2024, 1, 15)
                    Description = "時点取得テスト"
                    LineItems = createTestLineItems ()
                    UserId = "user1"
                    OccurredAt = DateTime.UtcNow
                }
            ]

            let! _ = repository.SaveAsync "TEST006" "JournalEntry" createEvents 0 "user1" None

            // 保存直後の時点を記録
            let afterFirstSave = DateTime.UtcNow

            // 少し待ってから承認イベントを追加
            do! Task.Delay(100)

            let approveEvents : JournalEntryEvent list = [
                JournalEntryApproved {
                    JournalEntryId = "TEST006"
                    ApprovedBy = "manager"
                    ApprovalComment = "承認"
                    OccurredAt = DateTime.UtcNow
                    UserId = "manager"
                }
            ]

            let! _ = repository.SaveAsync "TEST006" "JournalEntry" approveEvents 1 "manager" None

            // 最初の保存直後の時点までのイベントを取得（作成イベントのみ）
            let! eventsUntil = repository.GetEventsUntilAsync "TEST006" afterFirstSave

            eventsUntil |> should haveLength 1

            match eventsUntil.Head with
            | JournalEntryCreated data ->
                data.JournalEntryId |> should equal "TEST006"
            | _ ->
                failwith "Expected JournalEntryCreated event"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``削除イベントを保存・取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = EventStoreRepository(conn)

            let createEvents : JournalEntryEvent list = [
                JournalEntryCreated {
                    JournalEntryId = "TEST007"
                    EntryDate = DateTime(2024, 1, 15)
                    Description = "削除テスト"
                    LineItems = createTestLineItems ()
                    UserId = "user1"
                    OccurredAt = DateTime.UtcNow
                }
            ]

            let! _ = repository.SaveAsync "TEST007" "JournalEntry" createEvents 0 "user1" None

            let deleteEvents : JournalEntryEvent list = [
                JournalEntryDeleted {
                    JournalEntryId = "TEST007"
                    Reason = "入力ミス"
                    OccurredAt = DateTime.UtcNow
                    UserId = "user1"
                }
            ]

            let! _ = repository.SaveAsync "TEST007" "JournalEntry" deleteEvents 1 "user1" None

            let! events = repository.GetEventsAsync "TEST007"

            events |> should haveLength 2

            match events.[1] with
            | JournalEntryDeleted data ->
                data.Reason |> should equal "入力ミス"
            | _ ->
                failwith "Expected JournalEntryDeleted event"

            do! this.CleanupTestDataAsync()
        }
