module AccountingSystem.Tests.Integration.JournalEntryEventSourcingWithSnapshotControllerIntegrationTest

open System
open System.Net
open System.Net.Http
open System.Net.Http.Json
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open Microsoft.Extensions.DependencyInjection
open AccountingSystem.Infrastructure.Web.Dtos
open AccountingSystem.Infrastructure.Web.Controllers
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services
open AccountingSystem.Infrastructure.Adapters
open AccountingSystem.Tests.PostgresWebTestBase
open Npgsql
open Dapper

/// <summary>
/// 仕訳イベントソーシング（スナップショット最適化版）API 統合テスト
/// Testcontainers + WebApplicationFactory を使用した E2E テスト
/// PostgresWebTestBase を継承してコンテナとWebファクトリを管理
/// </summary>
type JournalEntryEventSourcingWithSnapshotControllerIntegrationTest() =
    inherit PostgresWebTestBase()

    let basePath = "/api/v1/journal-entries-with-snapshot"

    /// <summary>
    /// サブクラスで DI 設定をカスタマイズ
    /// </summary>
    override _.ConfigureServices(services: IServiceCollection, connStr: string) =
        // 既存の IEventStoreRepository を削除して新しいものに置き換え
        let eventStoreDescriptor =
            services
            |> Seq.tryFind (fun d -> d.ServiceType = typeof<IEventStoreRepository>)
        match eventStoreDescriptor with
        | Some d -> services.Remove(d) |> ignore
        | None -> ()

        services.AddScoped<IEventStoreRepository>(fun _ ->
            EventStoreRepositoryAdapter(connStr) :> IEventStoreRepository
        ) |> ignore

        // 既存の ISnapshotRepository を削除して新しいものに置き換え
        let snapshotDescriptor =
            services
            |> Seq.tryFind (fun d -> d.ServiceType = typeof<ISnapshotRepository>)
        match snapshotDescriptor with
        | Some d -> services.Remove(d) |> ignore
        | None -> ()

        services.AddScoped<ISnapshotRepository>(fun _ ->
            SnapshotRepositoryAdapter(connStr) :> ISnapshotRepository
        ) |> ignore

        // 既存の JournalEntryEventSourcingServiceWithSnapshot を削除して新しいものに置き換え
        let serviceDescriptor =
            services
            |> Seq.tryFind (fun d -> d.ServiceType = typeof<JournalEntryEventSourcingServiceWithSnapshot>)
        match serviceDescriptor with
        | Some d -> services.Remove(d) |> ignore
        | None -> ()

        // スナップショット間隔を 2 に設定（テスト用）
        services.AddScoped<JournalEntryEventSourcingServiceWithSnapshot>(fun sp ->
            let eventStoreRepo = sp.GetRequiredService<IEventStoreRepository>()
            let snapshotRepo = sp.GetRequiredService<ISnapshotRepository>()
            JournalEntryEventSourcingServiceWithSnapshot(eventStoreRepo, snapshotRepo, 2)
        ) |> ignore

    /// <summary>
    /// テスト用データのクリーンアップ
    /// </summary>
    member this.CleanupTestDataAsync() =
        task {
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let sql = """
                DELETE FROM event_store WHERE aggregate_id LIKE 'SNAP-TEST%';
                DELETE FROM snapshot_store WHERE aggregate_id LIKE 'SNAP-TEST%';
            """

            let! _ = connection.ExecuteAsync(sql)
            return ()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``新しい仕訳を作成できる``() : Task =
        task {
            do! this.CleanupTestDataAsync()

            // Arrange
            let request : CreateJournalEntryRequest = {
                Id = "SNAP-TEST-J-001"
                EntryDate = DateTime(2024, 1, 15)
                Description = "スナップショット統合テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 10000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 10000m }
                |]
            }

            // Act
            let! response = this.Client.PostAsJsonAsync(basePath, request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.Created

            let! created = response.Content.ReadFromJsonAsync<JournalEntryResponse>()

            created |> should not' (be null)
            created.Id |> should equal "SNAP-TEST-J-001"
            created.Description |> should equal "スナップショット統合テスト仕訳"
            created.Status |> should equal "Draft"
            created.Version |> should equal 1

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``仕訳を取得できる``() : Task =
        task {
            do! this.CleanupTestDataAsync()

            // Arrange - 先に仕訳を作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "SNAP-TEST-J-002"
                EntryDate = DateTime(2024, 1, 15)
                Description = "取得テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 5000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 5000m }
                |]
            }

            let! createResponse = this.Client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act
            let! response = this.Client.GetAsync($"{basePath}/SNAP-TEST-J-002")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! entry = response.Content.ReadFromJsonAsync<JournalEntryResponse>()

            entry |> should not' (be null)
            entry.Id |> should equal "SNAP-TEST-J-002"
            entry.Description |> should equal "取得テスト仕訳"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``存在しない仕訳を取得すると404が返る``() : Task =
        task {
            // Act
            let! response = this.Client.GetAsync($"{basePath}/NONEXISTENT")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.NotFound
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``仕訳を承認できる``() : Task =
        task {
            do! this.CleanupTestDataAsync()

            // Arrange - 先に仕訳を作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "SNAP-TEST-J-003"
                EntryDate = DateTime(2024, 1, 15)
                Description = "承認テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 8000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 8000m }
                |]
            }

            let! createResponse = this.Client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act
            let approveRequest : ApproveJournalEntryRequest = {
                ApprovedBy = "manager"
                ApprovalComment = "承認します"
            }

            let! response = this.Client.PostAsJsonAsync($"{basePath}/SNAP-TEST-J-003/approve", approveRequest)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! approved = response.Content.ReadFromJsonAsync<JournalEntryResponse>()

            approved |> should not' (be null)
            approved.Status |> should equal "Approved"
            approved.Version |> should equal 2

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``仕訳を削除できる``() : Task =
        task {
            do! this.CleanupTestDataAsync()

            // Arrange - 先に仕訳を作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "SNAP-TEST-J-004"
                EntryDate = DateTime(2024, 1, 15)
                Description = "削除テスト仕訳"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 6000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 6000m }
                |]
            }

            let! createResponse = this.Client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act
            let deleteRequest : DeleteJournalEntryRequest = {
                Reason = "テスト削除"
            }

            let request = new HttpRequestMessage(HttpMethod.Delete, $"{basePath}/SNAP-TEST-J-004")
            request.Content <- JsonContent.Create(deleteRequest)

            let! response = this.Client.SendAsync(request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.NoContent

            // 削除後に取得すると 404 になる
            let! getResponse = this.Client.GetAsync($"{basePath}/SNAP-TEST-J-004")
            getResponse.StatusCode |> should equal HttpStatusCode.NotFound

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``スナップショットが自動的に作成される``() : Task =
        task {
            do! this.CleanupTestDataAsync()

            // Arrange - 仕訳を作成（Version = 1）
            let createRequest : CreateJournalEntryRequest = {
                Id = "SNAP-TEST-J-005"
                EntryDate = DateTime(2024, 1, 15)
                Description = "スナップショット自動作成テスト"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 7000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 7000m }
                |]
            }

            let! createResponse = this.Client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act - 承認（Version = 2）→ スナップショット間隔が 2 なので作成されるはず
            let approveRequest : ApproveJournalEntryRequest = {
                ApprovedBy = "manager"
                ApprovalComment = "承認"
            }

            let! approveResponse = this.Client.PostAsJsonAsync($"{basePath}/SNAP-TEST-J-005/approve", approveRequest)
            approveResponse.StatusCode |> should equal HttpStatusCode.OK

            // Assert - スナップショットが作成されたか確認
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let sql = """
                SELECT COUNT(*) FROM snapshot_store
                WHERE aggregate_id = 'SNAP-TEST-J-005'
            """

            let! count = connection.ExecuteScalarAsync<int>(sql)
            count |> should equal 1

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``手動でスナップショットを作成できる``() : Task =
        task {
            do! this.CleanupTestDataAsync()

            // Arrange - 仕訳を作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "SNAP-TEST-J-006"
                EntryDate = DateTime(2024, 1, 15)
                Description = "手動スナップショットテスト"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 9000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 9000m }
                |]
            }

            let! createResponse = this.Client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // Act - 手動でスナップショットを作成
            let! response = this.Client.PostAsync($"{basePath}/SNAP-TEST-J-006/snapshot", null)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! result = response.Content.ReadFromJsonAsync<SnapshotResponse>()
            result.Success |> should equal true

            // スナップショットが作成されたか確認
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let sql = """
                SELECT COUNT(*) FROM snapshot_store
                WHERE aggregate_id = 'SNAP-TEST-J-006'
            """

            let! count = connection.ExecuteScalarAsync<int>(sql)
            count |> should equal 1

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``スナップショットを削除できる``() : Task =
        task {
            do! this.CleanupTestDataAsync()

            // Arrange - 仕訳を作成してスナップショットを作成
            let createRequest : CreateJournalEntryRequest = {
                Id = "SNAP-TEST-J-007"
                EntryDate = DateTime(2024, 1, 15)
                Description = "スナップショット削除テスト"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 11000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 11000m }
                |]
            }

            let! createResponse = this.Client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // 手動でスナップショットを作成
            let! snapshotResponse = this.Client.PostAsync($"{basePath}/SNAP-TEST-J-007/snapshot", null)
            snapshotResponse.StatusCode |> should equal HttpStatusCode.OK

            // Act - スナップショットを削除
            let! response = this.Client.DeleteAsync($"{basePath}/SNAP-TEST-J-007/snapshot")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.NoContent

            // スナップショットが削除されたか確認
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let sql = """
                SELECT COUNT(*) FROM snapshot_store
                WHERE aggregate_id = 'SNAP-TEST-J-007'
            """

            let! count = connection.ExecuteScalarAsync<int>(sql)
            count |> should equal 0

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``スナップショットから復元して正しく動作する``() : Task =
        task {
            do! this.CleanupTestDataAsync()

            // Arrange - 仕訳を作成（Version = 1）
            let createRequest : CreateJournalEntryRequest = {
                Id = "SNAP-TEST-J-008"
                EntryDate = DateTime(2024, 1, 15)
                Description = "スナップショット復元テスト"
                LineItems = [|
                    { AccountCode = "1000"; DebitCredit = "D"; Amount = 12000m }
                    { AccountCode = "2000"; DebitCredit = "C"; Amount = 12000m }
                |]
            }

            let! createResponse = this.Client.PostAsJsonAsync(basePath, createRequest)
            createResponse.StatusCode |> should equal HttpStatusCode.Created

            // 承認（Version = 2）→ スナップショット作成
            let approveRequest : ApproveJournalEntryRequest = {
                ApprovedBy = "manager"
                ApprovalComment = "承認"
            }

            let! approveResponse = this.Client.PostAsJsonAsync($"{basePath}/SNAP-TEST-J-008/approve", approveRequest)
            approveResponse.StatusCode |> should equal HttpStatusCode.OK

            // Act - 仕訳を取得（スナップショットから復元されるはず）
            let! response = this.Client.GetAsync($"{basePath}/SNAP-TEST-J-008")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! entry = response.Content.ReadFromJsonAsync<JournalEntryResponse>()
            entry.Version |> should equal 2
            entry.Status |> should equal "Approved"
            entry.Description |> should equal "スナップショット復元テスト"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``すべての仕訳を取得できる``() : Task =
        task {
            do! this.CleanupTestDataAsync()

            // Arrange - 複数の仕訳を作成
            for i in 1..3 do
                let createRequest : CreateJournalEntryRequest = {
                    Id = $"SNAP-TEST-J-LIST-{i}"
                    EntryDate = DateTime(2024, 1, 15)
                    Description = $"一覧テスト仕訳{i}"
                    LineItems = [|
                        { AccountCode = "1000"; DebitCredit = "D"; Amount = decimal (i * 1000) }
                        { AccountCode = "2000"; DebitCredit = "C"; Amount = decimal (i * 1000) }
                    |]
                }
                let! _ = this.Client.PostAsJsonAsync(basePath, createRequest)
                ()

            // Act
            let! response = this.Client.GetAsync(basePath)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! entries = response.Content.ReadFromJsonAsync<JournalEntryResponse array>()

            entries |> should not' (be null)
            entries.Length |> should be (greaterThanOrEqualTo 3)

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``存在しない仕訳のスナップショットを作成すると404が返る``() : Task =
        task {
            // Act
            let! response = this.Client.PostAsync($"{basePath}/NONEXISTENT/snapshot", null)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.NotFound
        }
