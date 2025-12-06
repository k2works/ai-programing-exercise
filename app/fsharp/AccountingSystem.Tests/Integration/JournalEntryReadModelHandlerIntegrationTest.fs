module AccountingSystem.Tests.Integration.JournalEntryReadModelHandlerIntegrationTest

open System
open System.Threading
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open DotNet.Testcontainers.Builders
open Testcontainers.PostgreSql
open Testcontainers.RabbitMq
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.EventHandlers
open AccountingSystem.Infrastructure.Adapters
open AccountingSystem.Infrastructure.Messaging
open AccountingSystem.Infrastructure.MigrationRunner

/// <summary>
/// JournalEntryReadModelHandler 統合テスト
/// RabbitMQ + PostgreSQL を使用した E2E テスト
/// </summary>
type JournalEntryReadModelHandlerIntegrationTest() =
    let mutable postgresContainer: PostgreSqlContainer = null
    let mutable rabbitMqContainer: RabbitMqContainer = null
    let mutable connectionString: string = null
    let mutable rabbitMqConfig: RabbitMqConfig = RabbitMqConfig.defaultConfig

    interface IAsyncLifetime with
        member _.InitializeAsync() : Task =
            task {
                // Docker ホストの設定（Windows Docker Desktop 用）
                let dockerHost =
                    match Environment.GetEnvironmentVariable("DOCKER_HOST") with
                    | null | "" -> "npipe://./pipe/docker_engine"
                    | host -> host

                Environment.SetEnvironmentVariable("DOCKER_HOST", dockerHost)

                // PostgreSQL コンテナの設定と起動
                postgresContainer <-
                    PostgreSqlBuilder()
                        .WithImage("postgres:16-alpine")
                        .WithDatabase("test_db")
                        .WithUsername("test")
                        .WithPassword("test")
                        .Build()

                // RabbitMQ コンテナの設定と起動
                rabbitMqContainer <-
                    RabbitMqBuilder()
                        .WithImage("rabbitmq:3-management-alpine")
                        .WithUsername("guest")
                        .WithPassword("guest")
                        .Build()

                // 並行で起動
                do! Task.WhenAll(
                    postgresContainer.StartAsync(),
                    rabbitMqContainer.StartAsync()
                )

                // 接続情報の取得
                connectionString <- postgresContainer.GetConnectionString()

                let rabbitMqHost = rabbitMqContainer.Hostname
                let rabbitMqPort = rabbitMqContainer.GetMappedPublicPort(5672)

                rabbitMqConfig <- {
                    HostName = rabbitMqHost
                    Port = int rabbitMqPort
                    UserName = "guest"
                    Password = "guest"
                    VirtualHost = "/"
                    ExchangeName = "accounting.events.readmodel.test"
                }

                // マイグレーションの実行
                migrateDatabase connectionString "PostgreSQL"
            }

        member _.DisposeAsync() : Task =
            task {
                if rabbitMqContainer <> null then
                    do! rabbitMqContainer.DisposeAsync().AsTask()
                if postgresContainer <> null then
                    do! postgresContainer.DisposeAsync().AsTask()
            }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``JournalEntryCreated イベントで Read Model が作成される``() : Task =
        task {
            // Arrange
            let readModelRepository = JournalEntryReadModelRepositoryAdapter(connectionString) :> IJournalEntryReadModelRepository
            let handler = JournalEntryReadModelHandler(readModelRepository)

            let event = JournalEntryCreated {
                JournalEntryId = "RM-TEST-001"
                EntryDate = DateTime(2024, 1, 15)
                Description = "Read Model テスト仕訳"
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

            let! readModel = readModelRepository.SelectByIdAsync "RM-TEST-001"
            readModel.IsSome |> should equal true

            let model = readModel.Value
            model.Id |> should equal "RM-TEST-001"
            model.Description |> should equal "Read Model テスト仕訳"
            model.Status |> should equal "Draft"
            model.Deleted |> should equal false

            // 明細も確認
            let! lines = readModelRepository.SelectLinesByJournalEntryIdAsync "RM-TEST-001"
            lines.Length |> should equal 2
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``JournalEntryApproved イベントで Read Model ステータスが更新される``() : Task =
        task {
            // Arrange
            let readModelRepository = JournalEntryReadModelRepositoryAdapter(connectionString) :> IJournalEntryReadModelRepository
            let handler = JournalEntryReadModelHandler(readModelRepository)

            // 先に仕訳を作成
            let createEvent = JournalEntryCreated {
                JournalEntryId = "RM-TEST-002"
                EntryDate = DateTime(2024, 1, 16)
                Description = "承認テスト仕訳"
                LineItems = [
                    { AccountCode = "1100"; DebitCredit = DebitCreditType.Debit; Amount = 30000m }
                    { AccountCode = "2100"; DebitCredit = DebitCreditType.Credit; Amount = 30000m }
                ]
                UserId = "user1"
                OccurredAt = DateTime(2024, 1, 16, 9, 0, 0)
            }

            let! _ = (handler :> IJournalEntryEventHandler).HandleAsync(createEvent)

            // Act - 承認イベント
            let approveEvent = JournalEntryApproved {
                JournalEntryId = "RM-TEST-002"
                ApprovedBy = "manager"
                ApprovalComment = "承認します"
                OccurredAt = DateTime(2024, 1, 16, 10, 0, 0)
                UserId = "manager"
            }

            let! result = (handler :> IJournalEntryEventHandler).HandleAsync(approveEvent)

            // Assert
            match result with
            | Ok () -> ()
            | Error e -> failwith $"Expected Ok but got Error: {e}"

            let! readModel = readModelRepository.SelectByIdAsync "RM-TEST-002"
            readModel.IsSome |> should equal true

            let model = readModel.Value
            model.Status |> should equal "Approved"
            model.ApprovedBy |> should equal (Some "manager")
            model.ApprovalComment |> should equal (Some "承認します")
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``JournalEntryDeleted イベントで Read Model が削除済みになる``() : Task =
        task {
            // Arrange
            let readModelRepository = JournalEntryReadModelRepositoryAdapter(connectionString) :> IJournalEntryReadModelRepository
            let handler = JournalEntryReadModelHandler(readModelRepository)

            // 先に仕訳を作成
            let createEvent = JournalEntryCreated {
                JournalEntryId = "RM-TEST-003"
                EntryDate = DateTime(2024, 1, 17)
                Description = "削除テスト仕訳"
                LineItems = [
                    { AccountCode = "1200"; DebitCredit = DebitCreditType.Debit; Amount = 20000m }
                    { AccountCode = "2200"; DebitCredit = DebitCreditType.Credit; Amount = 20000m }
                ]
                UserId = "user1"
                OccurredAt = DateTime(2024, 1, 17, 9, 0, 0)
            }

            let! _ = (handler :> IJournalEntryEventHandler).HandleAsync(createEvent)

            // Act - 削除イベント
            let deleteEvent = JournalEntryDeleted {
                JournalEntryId = "RM-TEST-003"
                Reason = "テスト削除"
                OccurredAt = DateTime(2024, 1, 17, 11, 0, 0)
                UserId = "user1"
            }

            let! result = (handler :> IJournalEntryEventHandler).HandleAsync(deleteEvent)

            // Assert
            match result with
            | Ok () -> ()
            | Error e -> failwith $"Expected Ok but got Error: {e}"

            let! readModel = readModelRepository.SelectByIdAsync "RM-TEST-003"
            readModel.IsSome |> should equal true

            let model = readModel.Value
            model.Deleted |> should equal true
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``RabbitMQ 経由でイベントを受信して Read Model が作成される``() : Task =
        task {
            // Arrange
            let readModelRepository = JournalEntryReadModelRepositoryAdapter(connectionString) :> IJournalEntryReadModelRepository
            let handler = JournalEntryReadModelHandler(readModelRepository)

            use publisher = new RabbitMqEventPublisher(rabbitMqConfig)
            use subscriber = new RabbitMqEventSubscriber(rabbitMqConfig, [ handler ])

            do! subscriber.StartAsync(CancellationToken.None)
            do! Task.Delay(500)

            // Act - RabbitMQ 経由でイベントをパブリッシュ
            let event = JournalEntryCreated {
                JournalEntryId = "RM-TEST-004"
                EntryDate = DateTime(2024, 1, 18)
                Description = "RabbitMQ経由テスト"
                LineItems = [
                    { AccountCode = "1300"; DebitCredit = DebitCreditType.Debit; Amount = 75000m }
                    { AccountCode = "2300"; DebitCredit = DebitCreditType.Credit; Amount = 75000m }
                ]
                UserId = "user1"
                OccurredAt = DateTime(2024, 1, 18, 10, 0, 0)
            }

            do! (publisher :> IEventPublisher).PublishAsync(event)

            // イベント処理を待機
            do! Task.Delay(2000)

            // Assert
            let! readModel = readModelRepository.SelectByIdAsync "RM-TEST-004"
            readModel.IsSome |> should equal true

            let model = readModel.Value
            model.Id |> should equal "RM-TEST-004"
            model.Description |> should equal "RabbitMQ経由テスト"

            do! subscriber.StopAsync()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``RabbitMQ 経由で複数イベントを順次処理できる``() : Task =
        task {
            // Arrange
            let readModelRepository = JournalEntryReadModelRepositoryAdapter(connectionString) :> IJournalEntryReadModelRepository
            let handler = JournalEntryReadModelHandler(readModelRepository)

            use publisher = new RabbitMqEventPublisher(rabbitMqConfig)
            use subscriber = new RabbitMqEventSubscriber(rabbitMqConfig, [ handler ])

            do! subscriber.StartAsync(CancellationToken.None)
            do! Task.Delay(500)

            // Act - 作成 → 承認 の順でイベントをパブリッシュ
            let createEvent = JournalEntryCreated {
                JournalEntryId = "RM-TEST-005"
                EntryDate = DateTime(2024, 1, 19)
                Description = "シーケンステスト"
                LineItems = [
                    { AccountCode = "1400"; DebitCredit = DebitCreditType.Debit; Amount = 100000m }
                    { AccountCode = "2400"; DebitCredit = DebitCreditType.Credit; Amount = 100000m }
                ]
                UserId = "user1"
                OccurredAt = DateTime(2024, 1, 19, 9, 0, 0)
            }

            do! (publisher :> IEventPublisher).PublishAsync(createEvent)
            do! Task.Delay(1000) // 作成イベントの処理を待機

            let approveEvent = JournalEntryApproved {
                JournalEntryId = "RM-TEST-005"
                ApprovedBy = "supervisor"
                ApprovalComment = "問題なし"
                OccurredAt = DateTime(2024, 1, 19, 10, 0, 0)
                UserId = "supervisor"
            }

            do! (publisher :> IEventPublisher).PublishAsync(approveEvent)
            do! Task.Delay(1000) // 承認イベントの処理を待機

            // Assert
            let! readModel = readModelRepository.SelectByIdAsync "RM-TEST-005"
            readModel.IsSome |> should equal true

            let model = readModel.Value
            model.Status |> should equal "Approved"
            model.ApprovedBy |> should equal (Some "supervisor")

            do! subscriber.StopAsync()
        }
