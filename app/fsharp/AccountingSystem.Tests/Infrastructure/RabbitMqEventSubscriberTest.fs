module AccountingSystem.Tests.Infrastructure.RabbitMqEventSubscriberTest

open System
open System.Threading
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Abstractions
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Infrastructure.Messaging
open AccountingSystem.Tests.RabbitMqTestBase

/// <summary>
/// テスト用のモックイベントハンドラー
/// 受信したイベントを記録する
/// </summary>
type MockEventHandler() =
    let mutable receivedEvents = ResizeArray<JournalEntryEvent>()
    let eventReceived = new ManualResetEventSlim(false)

    member _.ReceivedEvents = receivedEvents |> Seq.toList
    member _.EventReceived = eventReceived
    member _.WaitForEvent(timeout: TimeSpan) = eventReceived.Wait(timeout)
    member _.Reset() =
        receivedEvents.Clear()
        eventReceived.Reset()

    interface IJournalEntryEventHandler with
        member _.EventTypes = [ "JournalEntryCreated"; "JournalEntryApproved"; "JournalEntryDeleted" ]
        member this.HandleAsync(event: JournalEntryEvent) : Task<Result<unit, string>> =
            task {
                receivedEvents.Add(event)
                eventReceived.Set()
                return Ok ()
            }

/// <summary>
/// テスト用 IServiceScopeFactory を作成するヘルパー
/// </summary>
module TestServiceScopeFactory =
    /// <summary>
    /// 指定されたハンドラーを返す IServiceScopeFactory を作成
    /// </summary>
    let create (handlers: IJournalEntryEventHandler list) : IServiceScopeFactory =
        let services = ServiceCollection()
        handlers |> List.iter (fun h -> services.AddSingleton<IJournalEntryEventHandler>(h) |> ignore)
        let serviceProvider = services.BuildServiceProvider()

        { new IServiceScopeFactory with
            member _.CreateScope() =
                { new IServiceScope with
                    member _.ServiceProvider = serviceProvider
                    member _.Dispose() = ()
                }
        }

/// <summary>
/// RabbitMQ イベントサブスクライバー統合テスト
/// Testcontainers を使用した E2E テスト
/// RabbitMqTestBase を継承してコンテナのライフサイクルを管理
/// </summary>
type RabbitMqEventSubscriberTest() =
    inherit RabbitMqTestBase()

    let createSubscriber config handlers =
        let scopeFactory = TestServiceScopeFactory.create handlers
        let logger = NullLogger<RabbitMqEventSubscriber>.Instance
        new RabbitMqEventSubscriber(config, scopeFactory, logger)

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``パブリッシュされたイベントをサブスクライブできる``() : Task =
        task {
            // Arrange
            let mockHandler = MockEventHandler()
            use publisher = new RabbitMqEventPublisher(this.Config)
            use subscriber = createSubscriber this.Config [ mockHandler ]

            // サブスクライバーを開始
            do! subscriber.StartAsync(CancellationToken.None)

            // 少し待機（キューのバインドが完了するまで）
            do! Task.Delay(500)

            // Act - イベントをパブリッシュ
            let event = JournalEntryCreated {
                JournalEntryId = "SUB-TEST-001"
                EntryDate = DateTime(2024, 1, 15)
                Description = "サブスクライブテスト"
                LineItems = [
                    { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 10000m }
                    { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 10000m }
                ]
                UserId = "user1"
                OccurredAt = DateTime(2024, 1, 15, 10, 30, 0)
            }

            do! (publisher :> IEventPublisher).PublishAsync(event)

            // Assert - イベントが受信されるまで待機
            let received = mockHandler.WaitForEvent(TimeSpan.FromSeconds(10.0))
            received |> should equal true

            let receivedEvents = mockHandler.ReceivedEvents
            receivedEvents.Length |> should equal 1

            match receivedEvents.[0] with
            | JournalEntryCreated data ->
                data.JournalEntryId |> should equal "SUB-TEST-001"
                data.Description |> should equal "サブスクライブテスト"
                data.LineItems.Length |> should equal 2
            | _ ->
                failwith "JournalEntryCreated イベントが期待されました"

            // クリーンアップ
            do! subscriber.StopAsync(CancellationToken.None)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``複数のイベントをサブスクライブできる``() : Task =
        task {
            // Arrange
            let mockHandler = MockEventHandler()
            use publisher = new RabbitMqEventPublisher(this.Config)
            use subscriber = createSubscriber this.Config [ mockHandler ]

            do! subscriber.StartAsync(CancellationToken.None)
            do! Task.Delay(500)

            // Act - 複数のイベントをパブリッシュ
            let events = [
                JournalEntryCreated {
                    JournalEntryId = "SUB-TEST-002"
                    EntryDate = DateTime(2024, 1, 15)
                    Description = "テスト仕訳1"
                    LineItems = [
                        { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 5000m }
                        { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 5000m }
                    ]
                    UserId = "user1"
                    OccurredAt = DateTime(2024, 1, 15, 10, 0, 0)
                }
                JournalEntryApproved {
                    JournalEntryId = "SUB-TEST-002"
                    ApprovedBy = "manager"
                    ApprovalComment = "承認"
                    OccurredAt = DateTime(2024, 1, 15, 11, 0, 0)
                    UserId = "manager"
                }
            ]

            do! (publisher :> IEventPublisher).PublishManyAsync(events)

            // Assert - 両方のイベントが受信されるまで待機
            do! Task.Delay(2000) // 全イベント処理を待つ

            let receivedEvents = mockHandler.ReceivedEvents
            receivedEvents.Length |> should equal 2

            // クリーンアップ
            do! subscriber.StopAsync(CancellationToken.None)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``JournalEntryApproved イベントをサブスクライブできる``() : Task =
        task {
            // Arrange
            let mockHandler = MockEventHandler()
            use publisher = new RabbitMqEventPublisher(this.Config)
            use subscriber = createSubscriber this.Config [ mockHandler ]

            do! subscriber.StartAsync(CancellationToken.None)
            do! Task.Delay(500)

            // Act
            let event = JournalEntryApproved {
                JournalEntryId = "SUB-TEST-003"
                ApprovedBy = "supervisor"
                ApprovalComment = "問題なし"
                OccurredAt = DateTime(2024, 1, 16, 14, 0, 0)
                UserId = "supervisor"
            }

            do! (publisher :> IEventPublisher).PublishAsync(event)

            // Assert
            let received = mockHandler.WaitForEvent(TimeSpan.FromSeconds(10.0))
            received |> should equal true

            let receivedEvents = mockHandler.ReceivedEvents
            receivedEvents.Length |> should equal 1

            match receivedEvents.[0] with
            | JournalEntryApproved data ->
                data.JournalEntryId |> should equal "SUB-TEST-003"
                data.ApprovedBy |> should equal "supervisor"
                data.ApprovalComment |> should equal "問題なし"
            | _ ->
                failwith "JournalEntryApproved イベントが期待されました"

            do! subscriber.StopAsync(CancellationToken.None)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``JournalEntryDeleted イベントをサブスクライブできる``() : Task =
        task {
            // Arrange
            let mockHandler = MockEventHandler()
            use publisher = new RabbitMqEventPublisher(this.Config)
            use subscriber = createSubscriber this.Config [ mockHandler ]

            do! subscriber.StartAsync(CancellationToken.None)
            do! Task.Delay(500)

            // Act
            let event = JournalEntryDeleted {
                JournalEntryId = "SUB-TEST-004"
                Reason = "テスト削除"
                OccurredAt = DateTime(2024, 1, 17, 9, 0, 0)
                UserId = "user1"
            }

            do! (publisher :> IEventPublisher).PublishAsync(event)

            // Assert
            let received = mockHandler.WaitForEvent(TimeSpan.FromSeconds(10.0))
            received |> should equal true

            let receivedEvents = mockHandler.ReceivedEvents
            receivedEvents.Length |> should equal 1

            match receivedEvents.[0] with
            | JournalEntryDeleted data ->
                data.JournalEntryId |> should equal "SUB-TEST-004"
                data.Reason |> should equal "テスト削除"
            | _ ->
                failwith "JournalEntryDeleted イベントが期待されました"

            do! subscriber.StopAsync(CancellationToken.None)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``複数のハンドラーにイベントを配信できる``() : Task =
        task {
            // Arrange
            let mockHandler1 = MockEventHandler()
            let mockHandler2 = MockEventHandler()
            use publisher = new RabbitMqEventPublisher(this.Config)
            use subscriber = createSubscriber this.Config [ mockHandler1; mockHandler2 ]

            do! subscriber.StartAsync(CancellationToken.None)
            do! Task.Delay(500)

            // Act
            let event = JournalEntryCreated {
                JournalEntryId = "SUB-TEST-005"
                EntryDate = DateTime(2024, 1, 15)
                Description = "複数ハンドラーテスト"
                LineItems = [
                    { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 3000m }
                    { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 3000m }
                ]
                UserId = "user1"
                OccurredAt = DateTime(2024, 1, 15, 10, 0, 0)
            }

            do! (publisher :> IEventPublisher).PublishAsync(event)

            // Assert - 両方のハンドラーでイベントが受信される
            let received1 = mockHandler1.WaitForEvent(TimeSpan.FromSeconds(10.0))
            let received2 = mockHandler2.WaitForEvent(TimeSpan.FromSeconds(10.0))

            received1 |> should equal true
            received2 |> should equal true

            mockHandler1.ReceivedEvents.Length |> should equal 1
            mockHandler2.ReceivedEvents.Length |> should equal 1

            do! subscriber.StopAsync(CancellationToken.None)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``サブスクライバーを停止後は新しいイベントを受信しない``() : Task =
        task {
            // Arrange
            let mockHandler = MockEventHandler()
            use publisher = new RabbitMqEventPublisher(this.Config)
            use subscriber = createSubscriber this.Config [ mockHandler ]

            do! subscriber.StartAsync(CancellationToken.None)
            do! Task.Delay(500)

            // 最初のイベントをパブリッシュ
            let event1 = JournalEntryCreated {
                JournalEntryId = "SUB-TEST-006"
                EntryDate = DateTime(2024, 1, 15)
                Description = "停止前"
                LineItems = [
                    { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 1000m }
                    { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 1000m }
                ]
                UserId = "user1"
                OccurredAt = DateTime(2024, 1, 15, 10, 0, 0)
            }

            do! (publisher :> IEventPublisher).PublishAsync(event1)
            let received = mockHandler.WaitForEvent(TimeSpan.FromSeconds(10.0))
            received |> should equal true

            // Act - サブスクライバーを停止
            do! subscriber.StopAsync(CancellationToken.None)
            mockHandler.Reset()

            // 停止後にイベントをパブリッシュ
            let event2 = JournalEntryCreated {
                JournalEntryId = "SUB-TEST-007"
                EntryDate = DateTime(2024, 1, 15)
                Description = "停止後"
                LineItems = [
                    { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 2000m }
                    { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 2000m }
                ]
                UserId = "user1"
                OccurredAt = DateTime(2024, 1, 15, 11, 0, 0)
            }

            do! (publisher :> IEventPublisher).PublishAsync(event2)

            // Assert - 新しいイベントは受信されない（タイムアウト）
            let receivedAfterStop = mockHandler.WaitForEvent(TimeSpan.FromSeconds(2.0))
            receivedAfterStop |> should equal false
            mockHandler.ReceivedEvents.Length |> should equal 0
        }
