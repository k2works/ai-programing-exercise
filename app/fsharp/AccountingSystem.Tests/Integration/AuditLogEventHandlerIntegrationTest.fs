module AccountingSystem.Tests.Integration.AuditLogEventHandlerIntegrationTest

open System
open System.Threading
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging.Abstractions
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.EventHandlers
open AccountingSystem.Infrastructure.Adapters
open AccountingSystem.Infrastructure.Messaging
open AccountingSystem.Tests.DatabaseIntegrationTestBase

/// <summary>
/// テスト用 IServiceScopeFactory を作成するヘルパー
/// </summary>
module private TestServiceScopeFactory =
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
/// AuditLogEventHandler 統合テスト
/// RabbitMQ + PostgreSQL を使用した E2E テスト
/// </summary>
type AuditLogEventHandlerIntegrationTest() =
    inherit DatabaseIntegrationTestBase()

    let mutable rabbitMqConfig: RabbitMqConfig = RabbitMqConfig.defaultConfig

    let createSubscriber config handlers =
        let scopeFactory = TestServiceScopeFactory.create handlers
        let logger = NullLogger<RabbitMqEventSubscriber>.Instance
        new RabbitMqEventSubscriber(config, scopeFactory, logger)

    override this.InitializeAsync() =
        task {
            do! this.InitializeCoreAsync()
            rabbitMqConfig <- this.GetRabbitMqConfigWithExchange("accounting.events.auditlog.test")
        } :> Task

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``JournalEntryCreated イベントで監査ログが記録される``() : Task =
        task {
            // Arrange
            let auditLogRepository = AuditLogRepositoryAdapter(this.ConnectionString) :> IAuditLogRepository
            let handler = AuditLogEventHandler(auditLogRepository)

            let event = JournalEntryCreated {
                JournalEntryId = "AL-TEST-001"
                EntryDate = DateTime(2024, 1, 15)
                Description = "監査ログテスト仕訳"
                LineItems = [
                    { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 50000m }
                    { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 50000m }
                ]
                UserId = "user1"
                OccurredAt = DateTime(2024, 1, 15, 10, 0, 0)
            }

            // Act
            let! result = (handler :> IJournalEntryEventHandler).HandleAsync(event)

            // Assert
            match result with
            | Ok () -> ()
            | Error e -> failwith $"Expected Ok but got Error: {e}"

            let! auditLogs = auditLogRepository.FindByEntityAsync("JournalEntry", "AL-TEST-001")
            auditLogs.Length |> should greaterThanOrEqualTo 1

            let log = auditLogs |> List.head
            log.EntityType |> should equal "JournalEntry"
            log.EntityId |> should equal "AL-TEST-001"
            log.Action.ToString() |> should equal "Create"
            log.UserId |> should equal "user1"
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``JournalEntryApproved イベントで監査ログが記録される``() : Task =
        task {
            // Arrange
            let auditLogRepository = AuditLogRepositoryAdapter(this.ConnectionString) :> IAuditLogRepository
            let handler = AuditLogEventHandler(auditLogRepository)

            let event = JournalEntryApproved {
                JournalEntryId = "AL-TEST-002"
                ApprovedBy = "manager"
                ApprovalComment = "承認します"
                OccurredAt = DateTime(2024, 1, 16, 11, 0, 0)
                UserId = "manager"
            }

            // Act
            let! result = (handler :> IJournalEntryEventHandler).HandleAsync(event)

            // Assert
            match result with
            | Ok () -> ()
            | Error e -> failwith $"Expected Ok but got Error: {e}"

            let! auditLogs = auditLogRepository.FindByEntityAsync("JournalEntry", "AL-TEST-002")
            auditLogs.Length |> should greaterThanOrEqualTo 1

            let log = auditLogs |> List.head
            log.Action.ToString() |> should equal "Update"
            log.UserId |> should equal "manager"
            // OldValues は JSON 形式で保存される
            log.OldValues.IsSome |> should equal true
            log.OldValues.Value |> should haveSubstring "Draft"
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``JournalEntryDeleted イベントで監査ログが記録される``() : Task =
        task {
            // Arrange
            let auditLogRepository = AuditLogRepositoryAdapter(this.ConnectionString) :> IAuditLogRepository
            let handler = AuditLogEventHandler(auditLogRepository)

            let event = JournalEntryDeleted {
                JournalEntryId = "AL-TEST-003"
                Reason = "テスト削除"
                OccurredAt = DateTime(2024, 1, 17, 14, 0, 0)
                UserId = "user1"
            }

            // Act
            let! result = (handler :> IJournalEntryEventHandler).HandleAsync(event)

            // Assert
            match result with
            | Ok () -> ()
            | Error e -> failwith $"Expected Ok but got Error: {e}"

            let! auditLogs = auditLogRepository.FindByEntityAsync("JournalEntry", "AL-TEST-003")
            auditLogs.Length |> should greaterThanOrEqualTo 1

            let log = auditLogs |> List.head
            log.Action.ToString() |> should equal "Delete"
            log.UserId |> should equal "user1"
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``RabbitMQ 経由でイベントを受信して監査ログが記録される``() : Task =
        task {
            // Arrange
            let auditLogRepository = AuditLogRepositoryAdapter(this.ConnectionString) :> IAuditLogRepository
            let handler = AuditLogEventHandler(auditLogRepository)

            use publisher = new RabbitMqEventPublisher(rabbitMqConfig)
            use subscriber = createSubscriber rabbitMqConfig [ handler ]

            do! subscriber.StartAsync(CancellationToken.None)
            do! Task.Delay(500)

            // Act - RabbitMQ 経由でイベントをパブリッシュ
            let event = JournalEntryCreated {
                JournalEntryId = "AL-TEST-004"
                EntryDate = DateTime(2024, 1, 18)
                Description = "RabbitMQ経由監査ログテスト"
                LineItems = [
                    { AccountCode = "1500"; DebitCredit = DebitCreditType.Debit; Amount = 80000m }
                    { AccountCode = "2500"; DebitCredit = DebitCreditType.Credit; Amount = 80000m }
                ]
                UserId = "user2"
                OccurredAt = DateTime(2024, 1, 18, 10, 0, 0)
            }

            do! (publisher :> IEventPublisher).PublishAsync(event)

            // イベント処理を待機
            do! Task.Delay(2000)

            // Assert
            let! auditLogs = auditLogRepository.FindByEntityAsync("JournalEntry", "AL-TEST-004")
            auditLogs.Length |> should greaterThanOrEqualTo 1

            let log = auditLogs |> List.head
            log.EntityId |> should equal "AL-TEST-004"
            log.UserId |> should equal "user2"

            do! subscriber.StopAsync(CancellationToken.None)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``複数ハンドラーで同時にイベントを処理できる``() : Task =
        task {
            // Arrange
            let auditLogRepository = AuditLogRepositoryAdapter(this.ConnectionString) :> IAuditLogRepository
            let readModelRepository = JournalEntryReadModelRepositoryAdapter(this.ConnectionString) :> IJournalEntryReadModelRepository

            let auditLogHandler = AuditLogEventHandler(auditLogRepository) :> IJournalEntryEventHandler
            let readModelHandler = JournalEntryReadModelHandler(readModelRepository) :> IJournalEntryEventHandler

            use publisher = new RabbitMqEventPublisher(rabbitMqConfig)
            use subscriber = createSubscriber rabbitMqConfig [ auditLogHandler; readModelHandler ]

            do! subscriber.StartAsync(CancellationToken.None)
            do! Task.Delay(500)

            // Act - RabbitMQ 経由でイベントをパブリッシュ
            let event = JournalEntryCreated {
                JournalEntryId = "AL-TEST-005"
                EntryDate = DateTime(2024, 1, 19)
                Description = "複数ハンドラーテスト"
                LineItems = [
                    { AccountCode = "1600"; DebitCredit = DebitCreditType.Debit; Amount = 60000m }
                    { AccountCode = "2600"; DebitCredit = DebitCreditType.Credit; Amount = 60000m }
                ]
                UserId = "user3"
                OccurredAt = DateTime(2024, 1, 19, 9, 0, 0)
            }

            do! (publisher :> IEventPublisher).PublishAsync(event)

            // イベント処理を待機
            do! Task.Delay(2000)

            // Assert - 監査ログが記録されている
            let! auditLogs = auditLogRepository.FindByEntityAsync("JournalEntry", "AL-TEST-005")
            auditLogs.Length |> should greaterThanOrEqualTo 1

            // Assert - Read Model が作成されている
            let! readModel = readModelRepository.SelectByIdAsync "AL-TEST-005"
            readModel.IsSome |> should equal true
            readModel.Value.Description |> should equal "複数ハンドラーテスト"

            do! subscriber.StopAsync(CancellationToken.None)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``ユーザー別に監査ログを検索できる``() : Task =
        task {
            // Arrange
            let auditLogRepository = AuditLogRepositoryAdapter(this.ConnectionString) :> IAuditLogRepository
            let handler = AuditLogEventHandler(auditLogRepository)

            // 異なるユーザーで複数のイベントを処理
            let event1 = JournalEntryCreated {
                JournalEntryId = "AL-TEST-006"
                EntryDate = DateTime(2024, 1, 20)
                Description = "ユーザー検索テスト1"
                LineItems = [
                    { AccountCode = "1700"; DebitCredit = DebitCreditType.Debit; Amount = 10000m }
                    { AccountCode = "2700"; DebitCredit = DebitCreditType.Credit; Amount = 10000m }
                ]
                UserId = "search_user"
                OccurredAt = DateTime(2024, 1, 20, 9, 0, 0)
            }

            let event2 = JournalEntryCreated {
                JournalEntryId = "AL-TEST-007"
                EntryDate = DateTime(2024, 1, 20)
                Description = "ユーザー検索テスト2"
                LineItems = [
                    { AccountCode = "1800"; DebitCredit = DebitCreditType.Debit; Amount = 20000m }
                    { AccountCode = "2800"; DebitCredit = DebitCreditType.Credit; Amount = 20000m }
                ]
                UserId = "search_user"
                OccurredAt = DateTime(2024, 1, 20, 10, 0, 0)
            }

            // Act
            let! _ = (handler :> IJournalEntryEventHandler).HandleAsync(event1)
            let! _ = (handler :> IJournalEntryEventHandler).HandleAsync(event2)

            // Assert
            // AuditLog.create は DateTime.UtcNow を使用するため、現在の日時で検索
            let now = DateTime.UtcNow
            let! auditLogs = auditLogRepository.FindByUserAsync(
                "search_user",
                now.AddMinutes(-5.0),
                now.AddMinutes(5.0)
            )

            auditLogs.Length |> should greaterThanOrEqualTo 2

            auditLogs
            |> List.filter (fun log -> log.EntityId = "AL-TEST-006" || log.EntityId = "AL-TEST-007")
            |> List.length
            |> should equal 2
        }
