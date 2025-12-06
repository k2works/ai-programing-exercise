module AccountingSystem.Tests.Integration.JournalEntryEventSourcingControllerIntegrationTest

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
open AccountingSystem.Application.Port.Out
open AccountingSystem.Infrastructure.Adapters
open AccountingSystem.Infrastructure.MigrationRunner
open Npgsql
open Dapper

/// <summary>
/// 仕訳イベントソーシング API 統合テスト
/// Testcontainers + WebApplicationFactory を使用した E2E テスト
/// </summary>
type JournalEntryEventSourcingControllerIntegrationTest() =
    let mutable container: PostgreSqlContainer = null
    let mutable connectionString: string = null
    let mutable factory: WebApplicationFactory<Program> = null
    let mutable client: HttpClient = null

    /// <summary>
    /// テスト用データのクリーンアップ
    /// </summary>
    let cleanupTestData (connStr: string) =
        task {
            use connection = new NpgsqlConnection(connStr)
            do! connection.OpenAsync()

            let sql = """
                DELETE FROM event_store WHERE aggregate_id LIKE 'TEST%';
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
                                let descriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<IEventStoreRepository>)
                                match descriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                services.AddScoped<IEventStoreRepository>(fun _ ->
                                    EventStoreRepositoryAdapter(connectionString) :> IEventStoreRepository
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
                Id = "TEST-J-001"
                EntryDate = DateTime(2024, 1, 15)
                Description = "統合テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 10000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 10000m }
                |]
            }

            // Act
            let! response = client.PostAsJsonAsync("/api/v1/journal-entries", request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.Created

            let! created = response.Content.ReadFromJsonAsync<JournalEntryResponse>()

            created |> should not' (be null)
            created.Id |> should equal "TEST-J-001"
            created.Description |> should equal "統合テスト仕訳"
            created.Status |> should equal "Draft"
            created.TotalDebit |> should equal 10000m
            created.TotalCredit |> should equal 10000m

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``仕訳を取得できる``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 先に仕訳を作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "TEST-J-002"
                EntryDate = DateTime(2024, 1, 15)
                Description = "取得テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 5000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 5000m }
                |]
            }

            let! createResponse = client.PostAsJsonAsync("/api/v1/journal-entries", createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act
            let! response = client.GetAsync("/api/v1/journal-entries/TEST-J-002")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! entry = response.Content.ReadFromJsonAsync<JournalEntryResponse>()

            entry |> should not' (be null)
            entry.Id |> should equal "TEST-J-002"
            entry.Description |> should equal "取得テスト仕訳"

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``存在しない仕訳を取得すると404が返る``() : Task =
        task {
            // Act
            let! response = client.GetAsync("/api/v1/journal-entries/NONEXISTENT")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.NotFound
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``仕訳を承認できる``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 先に仕訳を作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "TEST-J-003"
                EntryDate = DateTime(2024, 1, 15)
                Description = "承認テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 8000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 8000m }
                |]
            }

            let! createResponse = client.PostAsJsonAsync("/api/v1/journal-entries", createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act
            let approveRequest : ApproveJournalEntryRequest = {
                ApprovedBy = "manager"
                ApprovalComment = "承認します"
            }

            let! response = client.PostAsJsonAsync("/api/v1/journal-entries/TEST-J-003/approve", approveRequest)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! approved = response.Content.ReadFromJsonAsync<JournalEntryResponse>()

            approved |> should not' (be null)
            approved.Status |> should equal "Approved"
            approved.Version |> should equal 2

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``承認済み仕訳を再度承認するとエラーになる``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 先に仕訳を作成して承認
            let createRequest : CreateJournalEntryRequest = {
                Id = "TEST-J-004"
                EntryDate = DateTime(2024, 1, 15)
                Description = "二重承認テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 7000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 7000m }
                |]
            }

            let! _ = client.PostAsJsonAsync("/api/v1/journal-entries", createRequest)

            let approveRequest : ApproveJournalEntryRequest = {
                ApprovedBy = "manager"
                ApprovalComment = "承認1回目"
            }

            let! _ = client.PostAsJsonAsync("/api/v1/journal-entries/TEST-J-004/approve", approveRequest)

            // Act - 2回目の承認
            let approveRequest2 : ApproveJournalEntryRequest = {
                ApprovedBy = "manager2"
                ApprovalComment = "承認2回目"
            }

            let! response = client.PostAsJsonAsync("/api/v1/journal-entries/TEST-J-004/approve", approveRequest2)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.UnprocessableEntity

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``仕訳を削除できる``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 先に仕訳を作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "TEST-J-005"
                EntryDate = DateTime(2024, 1, 15)
                Description = "削除テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 6000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 6000m }
                |]
            }

            let! createResponse = client.PostAsJsonAsync("/api/v1/journal-entries", createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act
            let deleteRequest : DeleteJournalEntryRequest = {
                Reason = "テスト削除"
            }

            let request = new HttpRequestMessage(HttpMethod.Delete, "/api/v1/journal-entries/TEST-J-005")
            request.Content <- JsonContent.Create(deleteRequest)

            let! response = client.SendAsync(request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.NoContent

            // 削除後に取得すると 404 になる
            let! getResponse = client.GetAsync("/api/v1/journal-entries/TEST-J-005")
            getResponse.StatusCode |> should equal HttpStatusCode.NotFound

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``すべての仕訳を取得できる``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 複数の仕訳を作成
            for i in 1..3 do
                let createRequest : CreateJournalEntryRequest = {
                    Id = $"TEST-J-LIST-{i}"
                    EntryDate = DateTime(2024, 1, 15)
                    Description = $"一覧テスト仕訳{i}"
                    LineItems = [|
                        { AccountCode = "1000"; DebitCredit = "D"; Amount = decimal (i * 1000) }
                        { AccountCode = "2000"; DebitCredit = "C"; Amount = decimal (i * 1000) }
                    |]
                }
                let! _ = client.PostAsJsonAsync("/api/v1/journal-entries", createRequest)
                ()

            // Act
            let! response = client.GetAsync("/api/v1/journal-entries")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! entries = response.Content.ReadFromJsonAsync<JournalEntryResponse array>()

            entries |> should not' (be null)
            entries.Length |> should be (greaterThanOrEqualTo 3)

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``貸借不一致の仕訳を作成するとエラーになる``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange
            let request : CreateJournalEntryRequest = {
                Id = "TEST-J-INVALID"
                EntryDate = DateTime(2024, 1, 15)
                Description = "貸借不一致仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 10000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 5000m }
                |]
            }

            // Act
            let! response = client.PostAsJsonAsync("/api/v1/journal-entries", request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.UnprocessableEntity

            do! cleanupTestData connectionString
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``イベントソーシングで仕訳の履歴が保持される``() : Task =
        task {
            do! cleanupTestData connectionString

            // Arrange - 仕訳を作成して承認
            let createRequest : CreateJournalEntryRequest = {
                Id = "TEST-J-HISTORY"
                EntryDate = DateTime(2024, 1, 15)
                Description = "履歴テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 15000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 15000m }
                |]
            }

            let! createResponse = client.PostAsJsonAsync("/api/v1/journal-entries", createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            let! created = createResponse.Content.ReadFromJsonAsync<JournalEntryResponse>()
            created.Version |> should equal 1

            // Act - 承認してバージョンが上がることを確認
            let approveRequest : ApproveJournalEntryRequest = {
                ApprovedBy = "manager"
                ApprovalComment = "承認"
            }

            let! approveResponse = client.PostAsJsonAsync("/api/v1/journal-entries/TEST-J-HISTORY/approve", approveRequest)
            approveResponse.StatusCode |> should equal HttpStatusCode.OK

            let! approved = approveResponse.Content.ReadFromJsonAsync<JournalEntryResponse>()
            approved.Version |> should equal 2
            approved.Status |> should equal "Approved"

            do! cleanupTestData connectionString
        }
