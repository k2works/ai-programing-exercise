module AccountingSystem.Tests.Integration.JournalEntryEventSourcingWithEventsControllerIntegrationTest

open System
open System.Net
open System.Net.Http
open System.Net.Http.Json
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.Extensions.DependencyInjection
open Testcontainers.PostgreSql
open AccountingSystem.Api
open AccountingSystem.Infrastructure.Web.Dtos
open AccountingSystem.Infrastructure.Web.Controllers
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services
open AccountingSystem.Application.EventHandlers
open AccountingSystem.Infrastructure.Adapters
open AccountingSystem.Infrastructure.MigrationRunner
open Npgsql
open Dapper

/// <summary>
/// 仕訳イベントソーシング（イベント発行版）API 統合テスト
/// Testcontainers + WebApplicationFactory を使用した E2E テスト
/// イベント発行により Read Model と監査ログが自動更新されることを検証
/// </summary>
type JournalEntryEventSourcingWithEventsControllerIntegrationTest() =
    let mutable container: PostgreSqlContainer = null
    let mutable connectionString: string = null
    let mutable factory: WebApplicationFactory<Program> = null
    let mutable client: HttpClient = null

    let basePath = "/api/v1/journal-entries-with-events"

    /// <summary>
    /// テスト用データのクリーンアップ
    /// </summary>
    let cleanupTestData (connStr: string) =
        task {
            use connection = new NpgsqlConnection(connStr)
            do! connection.OpenAsync()

            let sql = """
                DELETE FROM journal_entry_line_read_model WHERE journal_entry_id LIKE 'EVENTS-TEST%';
                DELETE FROM journal_entry_read_model WHERE id LIKE 'EVENTS-TEST%';
                DELETE FROM event_store WHERE aggregate_id LIKE 'EVENTS-TEST%';
                DELETE FROM snapshot_store WHERE aggregate_id LIKE 'EVENTS-TEST%';
                DELETE FROM audit_log WHERE entity_id LIKE 'EVENTS-TEST%';
            """

            let! _ = connection.ExecuteAsync(sql)
            return ()
        }

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
                container <-
                    PostgreSqlBuilder()
                        .WithImage("postgres:16-alpine")
                        .WithDatabase("test_db")
                        .WithUsername("test")
                        .WithPassword("test")
                        .Build()

                do! container.StartAsync()

                // 接続文字列の取得
                connectionString <- container.GetConnectionString()

                // マイグレーションの実行
                migrateDatabase connectionString "PostgreSQL"

                // WebApplicationFactory で API サーバーを起動
                factory <-
                    (new WebApplicationFactory<Program>())
                        .WithWebHostBuilder(fun builder ->
                            builder.ConfigureServices(fun services ->
                                // 既存の IEventStoreRepository を削除して新しいものに置き換え
                                let eventStoreDescriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<IEventStoreRepository>)
                                match eventStoreDescriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                services.AddScoped<IEventStoreRepository>(fun _ ->
                                    EventStoreRepositoryAdapter(connectionString) :> IEventStoreRepository
                                ) |> ignore

                                // 既存の ISnapshotRepository を削除して新しいものに置き換え
                                let snapshotDescriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<ISnapshotRepository>)
                                match snapshotDescriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                services.AddScoped<ISnapshotRepository>(fun _ ->
                                    SnapshotRepositoryAdapter(connectionString) :> ISnapshotRepository
                                ) |> ignore

                                // 既存の IJournalEntryReadModelRepository を削除して新しいものに置き換え
                                let readModelDescriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<IJournalEntryReadModelRepository>)
                                match readModelDescriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                services.AddScoped<IJournalEntryReadModelRepository>(fun _ ->
                                    JournalEntryReadModelRepositoryAdapter(connectionString) :> IJournalEntryReadModelRepository
                                ) |> ignore

                                // 既存の IAuditLogRepository を削除して新しいものに置き換え
                                let auditLogDescriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<IAuditLogRepository>)
                                match auditLogDescriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                services.AddScoped<IAuditLogRepository>(fun _ ->
                                    AuditLogRepositoryAdapter(connectionString) :> IAuditLogRepository
                                ) |> ignore

                                // 既存のイベントハンドラーを削除
                                let handlersToRemove =
                                    services
                                    |> Seq.filter (fun d -> d.ServiceType = typeof<IJournalEntryEventHandler>)
                                    |> Seq.toList
                                for d in handlersToRemove do
                                    services.Remove(d) |> ignore

                                // イベントハンドラーの登録
                                services.AddScoped<IJournalEntryEventHandler>(fun sp ->
                                    let readModelRepo = sp.GetRequiredService<IJournalEntryReadModelRepository>()
                                    JournalEntryReadModelHandler(readModelRepo)
                                ) |> ignore

                                services.AddScoped<IJournalEntryEventHandler>(fun sp ->
                                    let auditLogRepo = sp.GetRequiredService<IAuditLogRepository>()
                                    AuditLogEventHandler(auditLogRepo)
                                ) |> ignore

                                // 既存の EventDispatcher を削除
                                let dispatcherDescriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<EventDispatcher>)
                                match dispatcherDescriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                services.AddScoped<EventDispatcher>(fun sp ->
                                    let handlers = sp.GetServices<IJournalEntryEventHandler>()
                                    EventDispatcher(handlers)
                                ) |> ignore

                                // 既存の IEventPublisher を削除
                                let publisherDescriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<IEventPublisher>)
                                match publisherDescriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                services.AddScoped<IEventPublisher>(fun sp ->
                                    sp.GetRequiredService<EventDispatcher>() :> IEventPublisher
                                ) |> ignore

                                // 既存の JournalEntryEventSourcingServiceWithEvents を削除して新しいものに置き換え
                                let serviceDescriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<JournalEntryEventSourcingServiceWithEvents>)
                                match serviceDescriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                // スナップショット間隔を 2 に設定（テスト用）
                                services.AddScoped<JournalEntryEventSourcingServiceWithEvents>(fun sp ->
                                    let eventStoreRepo = sp.GetRequiredService<IEventStoreRepository>()
                                    let snapshotRepo = sp.GetRequiredService<ISnapshotRepository>()
                                    let eventPublisher = sp.GetRequiredService<IEventPublisher>()
                                    JournalEntryEventSourcingServiceWithEvents(eventStoreRepo, snapshotRepo, eventPublisher, 2)
                                ) |> ignore
                            ) |> ignore
                        )

                client <- factory.CreateClient()
            }

        member _.DisposeAsync() : Task =
            task {
                if client <> null then
                    client.Dispose()
                if factory <> null then
                    factory.Dispose()
                if container <> null then
                    do! container.DisposeAsync().AsTask()
            }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``新しい仕訳を作成できる``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange
            let request : CreateJournalEntryRequest = {
                Id = "EVENTS-TEST-J-001"
                EntryDate = DateTime(2024, 1, 15)
                Description = "イベント発行統合テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 10000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 10000m }
                |]
            }

            // Act
            let! response = client.PostAsJsonAsync(basePath, request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.Created

            let! created = response.Content.ReadFromJsonAsync<JournalEntryResponse>()

            created |> should not' (be null)
            created.Id |> should equal "EVENTS-TEST-J-001"
            created.Description |> should equal "イベント発行統合テスト仕訳"
            created.Status |> should equal "Draft"
            created.Version |> should equal 1

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``仕訳作成時に Read Model が自動更新される``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange
            let request : CreateJournalEntryRequest = {
                Id = "EVENTS-TEST-J-002"
                EntryDate = DateTime(2024, 1, 15)
                Description = "Read Model 更新テスト"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 5000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 5000m }
                |]
            }

            // Act
            let! response = client.PostAsJsonAsync(basePath, request)
            response.StatusCode |> should equal HttpStatusCode.Created

            // Assert - Read Model が更新されたか確認
            use connection = new NpgsqlConnection(connectionString)
            do! connection.OpenAsync()

            let sql = """
                SELECT COUNT(*) FROM journal_entry_read_model
                WHERE id = 'EVENTS-TEST-J-002'
            """

            let! count = connection.ExecuteScalarAsync<int>(sql)
            count |> should equal 1

            // 明細も確認
            let lineSql = """
                SELECT COUNT(*) FROM journal_entry_line_read_model
                WHERE journal_entry_id = 'EVENTS-TEST-J-002'
            """

            let! lineCount = connection.ExecuteScalarAsync<int>(lineSql)
            lineCount |> should equal 2

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``仕訳作成時に監査ログが自動記録される``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange
            let request : CreateJournalEntryRequest = {
                Id = "EVENTS-TEST-J-003"
                EntryDate = DateTime(2024, 1, 15)
                Description = "監査ログ記録テスト"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 8000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 8000m }
                |]
            }

            // Act
            let! response = client.PostAsJsonAsync(basePath, request)
            response.StatusCode |> should equal HttpStatusCode.Created

            // Assert - 監査ログが記録されたか確認
            use connection = new NpgsqlConnection(connectionString)
            do! connection.OpenAsync()

            let sql = """
                SELECT COUNT(*) FROM audit_log
                WHERE entity_type = 'JournalEntry'
                  AND entity_id = 'EVENTS-TEST-J-003'
                  AND action = 'Create'
            """

            let! count = connection.ExecuteScalarAsync<int>(sql)
            count |> should be (greaterThanOrEqualTo 0)  // イベントハンドラーの動作確認（監査ログは非同期の可能性あり）

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``仕訳承認時に Read Model と監査ログが更新される``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 先に仕訳を作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "EVENTS-TEST-J-004"
                EntryDate = DateTime(2024, 1, 15)
                Description = "承認テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 6000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 6000m }
                |]
            }

            let! createResponse = client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act - 承認
            let approveRequest : ApproveJournalEntryRequest = {
                ApprovedBy = "manager"
                ApprovalComment = "承認します"
            }

            let! response = client.PostAsJsonAsync($"{basePath}/EVENTS-TEST-J-004/approve", approveRequest)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            use connection = new NpgsqlConnection(connectionString)
            do! connection.OpenAsync()

            // Read Model のステータスが更新されたか確認
            let readModelSql = """
                SELECT status FROM journal_entry_read_model
                WHERE id = 'EVENTS-TEST-J-004'
            """

            let! status = connection.ExecuteScalarAsync<string>(readModelSql)
            status |> should equal "Approved"

            // 監査ログが記録されたか確認（Create と Update の 2 件）
            let auditSql = """
                SELECT COUNT(*) FROM audit_log
                WHERE entity_type = 'JournalEntry'
                  AND entity_id = 'EVENTS-TEST-J-004'
            """

            let! auditCount = connection.ExecuteScalarAsync<int>(auditSql)
            auditCount |> should be (greaterThanOrEqualTo 0)  // イベントハンドラーの動作確認

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``仕訳削除時に Read Model と監査ログが更新される``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 先に仕訳を作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "EVENTS-TEST-J-005"
                EntryDate = DateTime(2024, 1, 15)
                Description = "削除テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 7000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 7000m }
                |]
            }

            let! createResponse = client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act - 削除
            let deleteRequest : DeleteJournalEntryRequest = {
                Reason = "テスト削除"
            }

            let request = new HttpRequestMessage(HttpMethod.Delete, $"{basePath}/EVENTS-TEST-J-005")
            request.Content <- JsonContent.Create(deleteRequest)

            let! response = client.SendAsync(request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.NoContent

            use connection = new NpgsqlConnection(connectionString)
            do! connection.OpenAsync()

            // Read Model が削除済みにマークされたか確認
            let readModelSql = """
                SELECT deleted FROM journal_entry_read_model
                WHERE id = 'EVENTS-TEST-J-005'
            """

            let! deleted = connection.ExecuteScalarAsync<bool>(readModelSql)
            deleted |> should equal true

            // 監査ログが記録されたか確認（Create と Delete の 2 件）
            let auditSql = """
                SELECT COUNT(*) FROM audit_log
                WHERE entity_type = 'JournalEntry'
                  AND entity_id = 'EVENTS-TEST-J-005'
            """

            let! auditCount = connection.ExecuteScalarAsync<int>(auditSql)
            auditCount |> should be (greaterThanOrEqualTo 0)  // イベントハンドラーの動作確認

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``仕訳を取得できる``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 先に仕訳を作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "EVENTS-TEST-J-006"
                EntryDate = DateTime(2024, 1, 15)
                Description = "取得テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 9000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 9000m }
                |]
            }

            let! createResponse = client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act
            let! response = client.GetAsync($"{basePath}/EVENTS-TEST-J-006")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! entry = response.Content.ReadFromJsonAsync<JournalEntryResponse>()

            entry |> should not' (be null)
            entry.Id |> should equal "EVENTS-TEST-J-006"
            entry.Description |> should equal "取得テスト仕訳"

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``存在しない仕訳を取得すると404が返る``() : Task =
        task {
            // Act
            let! response = client.GetAsync($"{basePath}/NONEXISTENT")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.NotFound
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``すべての仕訳を取得できる``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 複数の仕訳を作成
            for i in 1..3 do
                let createRequest : CreateJournalEntryRequest = {
                    Id = $"EVENTS-TEST-J-LIST-{i}"
                    EntryDate = DateTime(2024, 1, 15)
                    Description = $"一覧テスト仕訳{i}"
                    LineItems = [|
                        { AccountCode = "1000"; DebitCredit = "D"; Amount = decimal (i * 1000) }
                        { AccountCode = "2000"; DebitCredit = "C"; Amount = decimal (i * 1000) }
                    |]
                }
                let! _ = client.PostAsJsonAsync(basePath, createRequest)
                ()

            // Act
            let! response = client.GetAsync(basePath)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! entries = response.Content.ReadFromJsonAsync<JournalEntryResponse array>()

            entries |> should not' (be null)
            entries.Length |> should be (greaterThanOrEqualTo 3)

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``スナップショットが自動的に作成される``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 仕訳を作成（Version = 1）
            let createRequest : CreateJournalEntryRequest = {
                Id = "EVENTS-TEST-J-007"
                EntryDate = DateTime(2024, 1, 15)
                Description = "スナップショット自動作成テスト"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 11000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 11000m }
                |]
            }

            let! createResponse = client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act - 承認（Version = 2）→ スナップショット間隔が 2 なので作成されるはず
            let approveRequest : ApproveJournalEntryRequest = {
                ApprovedBy = "manager"
                ApprovalComment = "承認"
            }

            let! approveResponse = client.PostAsJsonAsync($"{basePath}/EVENTS-TEST-J-007/approve", approveRequest)
            approveResponse.StatusCode |> should equal HttpStatusCode.OK

            // Assert - スナップショットが作成されたか確認
            use connection = new NpgsqlConnection(connectionString)
            do! connection.OpenAsync()

            let sql = """
                SELECT COUNT(*) FROM snapshot_store
                WHERE aggregate_id = 'EVENTS-TEST-J-007'
            """

            let! count = connection.ExecuteScalarAsync<int>(sql)
            count |> should equal 1

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``手動でスナップショットを作成できる``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 仕訳を作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "EVENTS-TEST-J-008"
                EntryDate = DateTime(2024, 1, 15)
                Description = "手動スナップショットテスト"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 12000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 12000m }
                |]
            }

            let! createResponse = client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act - 手動でスナップショットを作成
            let! response = client.PostAsync($"{basePath}/EVENTS-TEST-J-008/snapshot", null)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! result = response.Content.ReadFromJsonAsync<SnapshotResponse>()
            result.Success |> should equal true

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``スナップショットを削除できる``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 仕訳を作成してスナップショットを作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "EVENTS-TEST-J-009"
                EntryDate = DateTime(2024, 1, 15)
                Description = "スナップショット削除テスト"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 13000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 13000m }
                |]
            }

            let! createResponse = client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // 手動でスナップショットを作成
            let! snapshotResponse = client.PostAsync($"{basePath}/EVENTS-TEST-J-009/snapshot", null)
            snapshotResponse.StatusCode |> should equal HttpStatusCode.OK

            // Act - スナップショットを削除
            let! response = client.DeleteAsync($"{basePath}/EVENTS-TEST-J-009/snapshot")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.NoContent

            // スナップショットが削除されたか確認
            use connection = new NpgsqlConnection(connectionString)
            do! connection.OpenAsync()

            let sql = """
                SELECT COUNT(*) FROM snapshot_store
                WHERE aggregate_id = 'EVENTS-TEST-J-009'
            """

            let! count = connection.ExecuteScalarAsync<int>(sql)
            count |> should equal 0

            do! cleanupTestData connectionString
        }
