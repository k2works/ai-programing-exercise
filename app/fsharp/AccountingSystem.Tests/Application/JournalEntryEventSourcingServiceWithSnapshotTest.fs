module AccountingSystem.Tests.Application.JournalEntryEventSourcingServiceWithSnapshotTest

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open AccountingSystem.Domain.Aggregates
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services
open Xunit
open FsUnit.Xunit

/// <summary>
/// モック IEventStoreRepository
/// インメモリでイベントを管理
/// </summary>
type MockEventStoreRepository() =
    let mutable eventStore = Map.empty<string, (JournalEntryEvent * int) list>
    let mutable versionStore = Map.empty<string, int>

    member _.Reset() =
        eventStore <- Map.empty
        versionStore <- Map.empty

    member _.GetStoredEvents(aggregateId: string) =
        eventStore
        |> Map.tryFind aggregateId
        |> Option.defaultValue []
        |> List.map fst

    interface IEventStoreRepository with
        member _.SaveAsync
            (aggregateId: string)
            (_aggregateType: string)
            (events: JournalEntryEvent list)
            (expectedVersion: int)
            (_userId: string)
            (_correlationId: string option)
            : Task<Result<unit, string>> =
            task {
                let currentVersion = versionStore |> Map.tryFind aggregateId |> Option.defaultValue 0
                if currentVersion <> expectedVersion then
                    return Error $"バージョン競合: 期待={expectedVersion}, 現在={currentVersion}"
                else
                    let existingEvents = eventStore |> Map.tryFind aggregateId |> Option.defaultValue []
                    let newEvents = events |> List.mapi (fun i e -> (e, currentVersion + i + 1))
                    eventStore <- eventStore |> Map.add aggregateId (existingEvents @ newEvents)
                    versionStore <- versionStore |> Map.add aggregateId (currentVersion + events.Length)
                    return Ok ()
            }

        member _.GetEventsAsync (aggregateId: string) : Task<JournalEntryEvent list> =
            task {
                return eventStore
                    |> Map.tryFind aggregateId
                    |> Option.defaultValue []
                    |> List.map fst
            }

        member _.GetEventsUntilAsync (aggregateId: string) (pointInTime: DateTime) : Task<JournalEntryEvent list> =
            task {
                return eventStore
                    |> Map.tryFind aggregateId
                    |> Option.defaultValue []
                    |> List.map fst
                    |> List.filter (fun e ->
                        let occurredAt = JournalEntryEvent.getOccurredAt e
                        occurredAt <= pointInTime)
            }

        member _.GetCurrentVersionAsync (aggregateId: string) : Task<int> =
            task {
                return versionStore |> Map.tryFind aggregateId |> Option.defaultValue 0
            }

        member _.GetAggregateIdsAsync (_aggregateType: string) : Task<string list> =
            task {
                return eventStore |> Map.toList |> List.map fst
            }

        member _.GetEventsSinceVersionAsync (aggregateId: string) (sinceVersion: int) : Task<JournalEntryEvent list> =
            task {
                return eventStore
                    |> Map.tryFind aggregateId
                    |> Option.defaultValue []
                    |> List.filter (fun (_, v) -> v > sinceVersion)
                    |> List.map fst
            }

/// <summary>
/// モック ISnapshotRepository
/// インメモリでスナップショットを管理
/// </summary>
type MockSnapshotRepository() =
    let mutable snapshots = Map.empty<string, JournalEntryAggregate>

    member _.Reset() =
        snapshots <- Map.empty

    member _.GetStoredSnapshot(aggregateId: string) =
        snapshots |> Map.tryFind aggregateId

    member _.HasSnapshot(aggregateId: string) =
        snapshots |> Map.containsKey aggregateId

    interface ISnapshotRepository with
        member _.SaveSnapshotAsync
            (aggregateId: string)
            (_aggregateType: string)
            (aggregate: JournalEntryAggregate)
            : Task<Result<unit, string>> =
            task {
                snapshots <- snapshots |> Map.add aggregateId aggregate
                return Ok ()
            }

        member _.GetSnapshotAsync
            (aggregateId: string)
            (_aggregateType: string)
            : Task<JournalEntryAggregate option> =
            task {
                return snapshots |> Map.tryFind aggregateId
            }

        member _.DeleteSnapshotAsync
            (aggregateId: string)
            (_aggregateType: string)
            : Task<unit> =
            task {
                snapshots <- snapshots |> Map.remove aggregateId
            }

        member _.GetSnapshotVersionAsync
            (aggregateId: string)
            (_aggregateType: string)
            : Task<int> =
            task {
                return snapshots
                    |> Map.tryFind aggregateId
                    |> Option.map (fun a -> a.Version)
                    |> Option.defaultValue 0
            }

/// <summary>
/// JournalEntryEventSourcingServiceWithSnapshot 単体テスト
/// モックリポジトリを使用
/// </summary>
type JournalEntryEventSourcingServiceWithSnapshotTest() =

    let createTestLineItems () : JournalEntryLineItem list =
        [
            { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 10000m }
            { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 10000m }
        ]

    let createService (eventStore: MockEventStoreRepository) (snapshotStore: MockSnapshotRepository) (snapshotInterval: int option) =
        match snapshotInterval with
        | Some interval -> JournalEntryEventSourcingServiceWithSnapshot(eventStore, snapshotStore, interval)
        | None -> JournalEntryEventSourcingServiceWithSnapshot(eventStore, snapshotStore)

    [<Fact>]
    member _.``仕訳を作成できる``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            let service = createService eventStore snapshotStore None :> IJournalEntryEventSourcingUseCase

            let! result = service.CreateJournalEntryAsync
                            "TEST001"
                            (DateTime(2024, 1, 15))
                            "テスト仕訳"
                            (createTestLineItems ())
                            "user1"

            match result with
            | Ok aggregate ->
                aggregate.Id |> should equal "TEST001"
                aggregate.Description |> should equal "テスト仕訳"
                aggregate.Version |> should equal 1
                aggregate.Status |> should equal JournalEntryStatus.Draft
            | Error msg ->
                failwith $"Create failed: {msg}"

            // イベントが保存されているか確認
            let storedEvents = eventStore.GetStoredEvents "TEST001"
            storedEvents |> should haveLength 1
        }

    [<Fact>]
    member _.``仕訳を取得できる``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            let service = createService eventStore snapshotStore None :> IJournalEntryEventSourcingUseCase

            let! _ = service.CreateJournalEntryAsync
                        "TEST002"
                        (DateTime(2024, 1, 15))
                        "取得テスト仕訳"
                        (createTestLineItems ())
                        "user1"

            let! aggregateOpt = service.GetJournalEntryAsync "TEST002"

            match aggregateOpt with
            | Some aggregate ->
                aggregate.Id |> should equal "TEST002"
                aggregate.Description |> should equal "取得テスト仕訳"
            | None ->
                failwith "Expected aggregate but got None"
        }

    [<Fact>]
    member _.``仕訳を承認できる``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            let service = createService eventStore snapshotStore None :> IJournalEntryEventSourcingUseCase

            let! _ = service.CreateJournalEntryAsync
                        "TEST003"
                        (DateTime(2024, 1, 15))
                        "承認テスト仕訳"
                        (createTestLineItems ())
                        "user1"

            let! result = service.ApproveJournalEntryAsync "TEST003" "manager" "承認します"

            match result with
            | Ok aggregate ->
                aggregate.Status |> should equal JournalEntryStatus.Approved
                aggregate.Version |> should equal 2
            | Error msg ->
                failwith $"Approve failed: {msg}"

            // イベントが2つ保存されているか確認
            let storedEvents = eventStore.GetStoredEvents "TEST003"
            storedEvents |> should haveLength 2
        }

    [<Fact>]
    member _.``仕訳を削除できる``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            let service = createService eventStore snapshotStore None :> IJournalEntryEventSourcingUseCase

            let! _ = service.CreateJournalEntryAsync
                        "TEST004"
                        (DateTime(2024, 1, 15))
                        "削除テスト仕訳"
                        (createTestLineItems ())
                        "user1"

            let! result = service.DeleteJournalEntryAsync "TEST004" "入力ミス" "user1"

            match result with
            | Ok aggregate ->
                aggregate.Deleted |> should equal true
                aggregate.Version |> should equal 2
            | Error msg ->
                failwith $"Delete failed: {msg}"
        }

    [<Fact>]
    member _.``スナップショットが自動的に作成される``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            // スナップショット間隔を 2 に設定
            let serviceWithSnapshot = createService eventStore snapshotStore (Some 2)
            let service = serviceWithSnapshot :> IJournalEntryEventSourcingUseCase

            // 作成（Version = 1）
            let! _ = service.CreateJournalEntryAsync
                        "TEST005"
                        (DateTime(2024, 1, 15))
                        "スナップショット作成テスト"
                        (createTestLineItems ())
                        "user1"

            // この時点ではスナップショットなし
            snapshotStore.HasSnapshot "TEST005" |> should equal false

            // 承認（Version = 2）→ スナップショットが作成されるはず
            let! _ = service.ApproveJournalEntryAsync "TEST005" "manager" "承認"

            // スナップショットが作成されたか確認
            snapshotStore.HasSnapshot "TEST005" |> should equal true

            let snapshot = snapshotStore.GetStoredSnapshot "TEST005"
            match snapshot with
            | Some s ->
                s.Version |> should equal 2
                s.Status |> should equal JournalEntryStatus.Approved
            | None ->
                failwith "Expected snapshot to be created"
        }

    [<Fact>]
    member _.``スナップショットからの復元が正しく動作する``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            // スナップショット間隔を 2 に設定
            let service = createService eventStore snapshotStore (Some 2) :> IJournalEntryEventSourcingUseCase

            // 作成（Version = 1）
            let! _ = service.CreateJournalEntryAsync
                        "TEST006"
                        (DateTime(2024, 1, 15))
                        "復元テスト仕訳"
                        (createTestLineItems ())
                        "user1"

            // 承認（Version = 2）→ スナップショット作成
            let! _ = service.ApproveJournalEntryAsync "TEST006" "manager" "承認"

            // 再度取得（スナップショットから復元されるはず）
            let! aggregateOpt = service.GetJournalEntryAsync "TEST006"

            match aggregateOpt with
            | Some aggregate ->
                aggregate.Version |> should equal 2
                aggregate.Status |> should equal JournalEntryStatus.Approved
                aggregate.Description |> should equal "復元テスト仕訳"
            | None ->
                failwith "Expected aggregate but got None"
        }

    [<Fact>]
    member _.``スナップショット後のイベントも正しく適用される``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            // スナップショット間隔を 2 に設定
            let service = createService eventStore snapshotStore (Some 2) :> IJournalEntryEventSourcingUseCase

            // 作成（Version = 1）
            let! _ = service.CreateJournalEntryAsync
                        "TEST007"
                        (DateTime(2024, 1, 15))
                        "追加イベントテスト"
                        (createTestLineItems ())
                        "user1"

            // 承認（Version = 2）→ スナップショット作成
            let! _ = service.ApproveJournalEntryAsync "TEST007" "manager" "承認"

            // 削除（Version = 3）→ スナップショット後のイベント
            let! deleteResult = service.DeleteJournalEntryAsync "TEST007" "取消" "user1"

            match deleteResult with
            | Ok aggregate ->
                aggregate.Version |> should equal 3
                aggregate.Deleted |> should equal true
                aggregate.Status |> should equal JournalEntryStatus.Approved
            | Error msg ->
                failwith $"Delete failed: {msg}"

            // 再度取得してスナップショット + 追加イベントから正しく復元されるか確認
            let! aggregateOpt = service.GetJournalEntryAsync "TEST007"

            match aggregateOpt with
            | Some aggregate ->
                aggregate.Version |> should equal 3
                aggregate.Deleted |> should equal true
            | None ->
                failwith "Expected aggregate but got None"
        }

    [<Fact>]
    member _.``削除時にスナップショットも削除される``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            // スナップショット間隔を 2 に設定
            let service = createService eventStore snapshotStore (Some 2) :> IJournalEntryEventSourcingUseCase

            // 作成（Version = 1）
            let! _ = service.CreateJournalEntryAsync
                        "TEST008"
                        (DateTime(2024, 1, 15))
                        "スナップショット削除テスト"
                        (createTestLineItems ())
                        "user1"

            // 承認（Version = 2）→ スナップショット作成
            let! _ = service.ApproveJournalEntryAsync "TEST008" "manager" "承認"

            // スナップショットが作成されたか確認
            snapshotStore.HasSnapshot "TEST008" |> should equal true

            // 削除
            let! _ = service.DeleteJournalEntryAsync "TEST008" "取消" "user1"

            // スナップショットが削除されたか確認
            snapshotStore.HasSnapshot "TEST008" |> should equal false
        }

    [<Fact>]
    member _.``手動スナップショット作成が動作する``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            let serviceWithSnapshot = createService eventStore snapshotStore (Some 100) // 高い間隔で自動作成を防ぐ
            let service = serviceWithSnapshot :> IJournalEntryEventSourcingUseCase

            // 作成
            let! _ = service.CreateJournalEntryAsync
                        "TEST009"
                        (DateTime(2024, 1, 15))
                        "手動スナップショットテスト"
                        (createTestLineItems ())
                        "user1"

            // この時点ではスナップショットなし
            snapshotStore.HasSnapshot "TEST009" |> should equal false

            // 手動でスナップショットを作成
            let! result = serviceWithSnapshot.CreateSnapshotAsync "TEST009"

            match result with
            | Ok () -> ()
            | Error msg -> failwith $"Manual snapshot creation failed: {msg}"

            // スナップショットが作成されたか確認
            snapshotStore.HasSnapshot "TEST009" |> should equal true

            let snapshot = snapshotStore.GetStoredSnapshot "TEST009"
            match snapshot with
            | Some s ->
                s.Version |> should equal 1
            | None ->
                failwith "Expected snapshot to be created manually"
        }

    [<Fact>]
    member _.``すべての仕訳を取得できる``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            let service = createService eventStore snapshotStore None :> IJournalEntryEventSourcingUseCase

            // 複数の仕訳を作成
            for i in 1..3 do
                let! _ = service.CreateJournalEntryAsync
                            $"TEST010-{i}"
                            (DateTime(2024, 1, 15))
                            $"一覧テスト{i}"
                            (createTestLineItems ())
                            "user1"
                ()

            let! aggregates = service.GetAllJournalEntriesAsync()

            aggregates |> should haveLength 3
        }

    [<Fact>]
    member _.``削除済みの仕訳は一覧に含まれない``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            let service = createService eventStore snapshotStore None :> IJournalEntryEventSourcingUseCase

            // 仕訳を作成
            let! _ = service.CreateJournalEntryAsync
                        "TEST011"
                        (DateTime(2024, 1, 15))
                        "削除除外テスト"
                        (createTestLineItems ())
                        "user1"

            // 削除
            let! _ = service.DeleteJournalEntryAsync "TEST011" "テスト削除" "user1"

            let! aggregates = service.GetAllJournalEntriesAsync()

            let testAggregates = aggregates |> List.filter (fun a -> a.Id = "TEST011")
            testAggregates |> should haveLength 0
        }

    [<Fact>]
    member _.``存在しない仕訳を取得するとNoneが返る``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            let service = createService eventStore snapshotStore None :> IJournalEntryEventSourcingUseCase

            let! aggregateOpt = service.GetJournalEntryAsync "NON_EXISTENT"

            aggregateOpt |> should equal None
        }

    [<Fact>]
    member _.``存在しない仕訳を承認するとエラーが返る``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            let service = createService eventStore snapshotStore None :> IJournalEntryEventSourcingUseCase

            let! result = service.ApproveJournalEntryAsync "NON_EXISTENT" "manager" "承認"

            match result with
            | Ok _ -> failwith "Expected error but got Ok"
            | Error msg -> msg |> should equal "仕訳が見つかりません"
        }

    [<Fact>]
    member _.``存在しない仕訳を削除するとエラーが返る``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            let service = createService eventStore snapshotStore None :> IJournalEntryEventSourcingUseCase

            let! result = service.DeleteJournalEntryAsync "NON_EXISTENT" "理由" "user1"

            match result with
            | Ok _ -> failwith "Expected error but got Ok"
            | Error msg -> msg |> should equal "仕訳が見つかりません"
        }

    [<Fact>]
    member _.``既に承認済みの仕訳を再承認するとエラーが返る``() =
        task {
            let eventStore = MockEventStoreRepository()
            let snapshotStore = MockSnapshotRepository()
            let service = createService eventStore snapshotStore None :> IJournalEntryEventSourcingUseCase

            // 仕訳を作成
            let! _ = service.CreateJournalEntryAsync
                        "TEST012"
                        (DateTime(2024, 1, 15))
                        "再承認テスト"
                        (createTestLineItems ())
                        "user1"

            // 承認
            let! _ = service.ApproveJournalEntryAsync "TEST012" "manager" "承認"

            // 再度承認しようとするとエラー
            let! result = service.ApproveJournalEntryAsync "TEST012" "manager2" "再承認"

            match result with
            | Ok _ -> failwith "Expected error but got Ok"
            | Error msg -> msg |> should haveSubstring "承認"
        }
