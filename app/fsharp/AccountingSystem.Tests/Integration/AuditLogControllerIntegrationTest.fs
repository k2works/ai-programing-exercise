module AccountingSystem.Tests.Integration.AuditLogControllerIntegrationTest

open System
open System.Net
open System.Net.Http
open System.Net.Http.Json
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.Extensions.DependencyInjection
open DotNet.Testcontainers.Builders
open Testcontainers.PostgreSql
open AccountingSystem.Api
open AccountingSystem.Infrastructure.Web.Dtos
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services
open AccountingSystem.Infrastructure.Adapters
open AccountingSystem.Infrastructure.MigrationRunner
open Npgsql
open Dapper

/// <summary>
/// 監査ログ API 統合テスト
/// Testcontainers + WebApplicationFactory を使用した E2E テスト
/// </summary>
type AuditLogControllerIntegrationTest() =
    let mutable container: PostgreSqlContainer = null
    let mutable connectionString: string = null
    let mutable factory: WebApplicationFactory<Program> = null
    let mutable client: HttpClient = null

    /// <summary>
    /// テスト用の監査ログデータを挿入
    /// </summary>
    let insertTestData (connStr: string) =
        task {
            use connection = new NpgsqlConnection(connStr)
            do! connection.OpenAsync()

            let sql = """
                INSERT INTO audit_log (
                    entity_type, entity_id, action, user_id, user_name,
                    timestamp, old_values, new_values, changes, reason,
                    ip_address, user_agent
                )
                VALUES
                    ('Account', 'TEST001', 'CREATE', 'user1', 'テストユーザー1',
                     '2024-01-15 10:00:00', NULL, '{"name": "現金"}'::jsonb, '{"name": "現金"}'::jsonb, NULL,
                     '192.168.1.1', 'TestAgent/1.0'),
                    ('Account', 'TEST001', 'UPDATE', 'user1', 'テストユーザー1',
                     '2024-01-16 10:00:00', '{"name": "現金"}'::jsonb, '{"name": "現金及び預金"}'::jsonb, NULL, '名称変更',
                     '192.168.1.1', 'TestAgent/1.0'),
                    ('Journal', 'J-2024-0001', 'CREATE', 'user2', 'テストユーザー2',
                     '2024-01-17 10:00:00', NULL, '{"amount": 50000}'::jsonb, '{"amount": 50000}'::jsonb, NULL,
                     '192.168.1.2', 'TestAgent/2.0'),
                    ('Account', 'TEST002', 'DELETE', 'admin', '管理者',
                     '2024-01-18 10:00:00', '{"name": "廃止科目"}'::jsonb, NULL, NULL, '科目統合',
                     '10.0.0.1', 'AdminAgent/1.0')
                ON CONFLICT DO NOTHING;
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

                // PostgreSQLコンテナの設定と起動
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

                // テストデータを挿入
                do! insertTestData connectionString

                // WebApplicationFactory で API サーバーを起動
                factory <-
                    (new WebApplicationFactory<Program>())
                        .WithWebHostBuilder(fun builder ->
                            builder.ConfigureServices(fun services ->
                                // 既存の IAuditLogRepository を削除して新しいものに置き換え
                                let repoDescriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<IAuditLogRepository>)
                                match repoDescriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                // 既存の IAuditLogUseCase を削除して新しいものに置き換え
                                let useCaseDescriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<IAuditLogUseCase>)
                                match useCaseDescriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                // 接続文字列を使用して Repository を登録
                                services.AddScoped<IAuditLogRepository>(fun _ ->
                                    AuditLogRepositoryAdapter(connectionString) :> IAuditLogRepository
                                ) |> ignore

                                // UseCase を登録
                                services.AddScoped<IAuditLogUseCase, AuditLogService>() |> ignore
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
    member _.``エンティティ別の監査ログを取得できる``() : Task =
        task {
            // Arrange & Act
            let! response = client.GetAsync("/api/v1/audit-logs/entity/Account/TEST001")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! logs = response.Content.ReadFromJsonAsync<AuditLogResponse array>()

            logs |> should not' (be null)
            logs.Length |> should equal 2
            logs |> Array.forall (fun l -> l.EntityType = "Account" && l.EntityId = "TEST001")
                |> should equal true
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``ユーザー別の監査ログを期間指定で取得できる``() : Task =
        task {
            // Arrange
            let userId = "user1"
            let startDate = "2024-01-01T00:00:00"
            let endDate = "2024-01-31T23:59:59"

            // Act
            let! response = client.GetAsync($"/api/v1/audit-logs/user/{userId}?startDate={startDate}&endDate={endDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! logs = response.Content.ReadFromJsonAsync<AuditLogResponse array>()

            logs |> should not' (be null)
            logs.Length |> should equal 2
            logs |> Array.forall (fun l -> l.UserId = userId)
                |> should equal true
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``期間内のすべての監査ログを取得できる``() : Task =
        task {
            // Arrange
            let startDate = "2024-01-15T00:00:00"
            let endDate = "2024-01-17T23:59:59"

            // Act
            let! response = client.GetAsync($"/api/v1/audit-logs?startDate={startDate}&endDate={endDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! logs = response.Content.ReadFromJsonAsync<AuditLogResponse array>()

            logs |> should not' (be null)
            logs.Length |> should equal 3
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``アクション種別で監査ログを検索できる``() : Task =
        task {
            // Arrange
            let action = "CREATE"
            let startDate = "2024-01-01T00:00:00"
            let endDate = "2024-01-31T23:59:59"

            // Act
            let! response = client.GetAsync($"/api/v1/audit-logs/action/{action}?startDate={startDate}&endDate={endDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! logs = response.Content.ReadFromJsonAsync<AuditLogResponse array>()

            logs |> should not' (be null)
            logs.Length |> should equal 2
            logs |> Array.forall (fun l -> l.Action = "CREATE")
                |> should equal true
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``DELETE操作の監査ログを検索できる``() : Task =
        task {
            // Arrange
            let action = "DELETE"
            let startDate = "2024-01-01T00:00:00"
            let endDate = "2024-01-31T23:59:59"

            // Act
            let! response = client.GetAsync($"/api/v1/audit-logs/action/{action}?startDate={startDate}&endDate={endDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! logs = response.Content.ReadFromJsonAsync<AuditLogResponse array>()

            logs |> should not' (be null)
            logs.Length |> should equal 1
            logs.[0].Action |> should equal "DELETE"
            logs.[0].EntityId |> should equal "TEST002"
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``監査ログを記録できる``() : Task =
        task {
            // Arrange
            let request : CreateAuditLogRequest = {
                EntityType = "Account"
                EntityId = "NEW001"
                Action = "CREATE"
                UserId = "testuser"
                UserName = "テスト登録ユーザー"
                OldValues = None
                NewValues = Some "{\"name\": \"新規科目\"}"
                Changes = Some "{\"name\": \"新規科目\"}"
                Reason = Some "統合テスト"
                IpAddress = Some "127.0.0.1"
            }

            // Act
            let! response = client.PostAsJsonAsync("/api/v1/audit-logs", request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.Created

            let! created = response.Content.ReadFromJsonAsync<AuditLogResponse>()

            created |> should not' (be null)
            created.EntityType |> should equal "Account"
            created.EntityId |> should equal "NEW001"
            created.Action |> should equal "CREATE"
            created.UserId |> should equal "testuser"
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``無効なアクション種別で400が返る``() : Task =
        task {
            // Arrange
            let request : CreateAuditLogRequest = {
                EntityType = "Account"
                EntityId = "TEST999"
                Action = "INVALID"  // 無効なアクション
                UserId = "testuser"
                UserName = "テストユーザー"
                OldValues = None
                NewValues = None
                Changes = None
                Reason = None
                IpAddress = None
            }

            // Act
            let! response = client.PostAsJsonAsync("/api/v1/audit-logs", request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.BadRequest
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``存在しないエンティティの監査ログは空配列を返す``() : Task =
        task {
            // Arrange & Act
            let! response = client.GetAsync("/api/v1/audit-logs/entity/Account/NONEXISTENT")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! logs = response.Content.ReadFromJsonAsync<AuditLogResponse array>()

            logs |> should not' (be null)
            logs.Length |> should equal 0
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``監査ログのサマリーが正しく生成される``() : Task =
        task {
            // Arrange & Act
            let! response = client.GetAsync("/api/v1/audit-logs/entity/Account/TEST001")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! logs = response.Content.ReadFromJsonAsync<AuditLogResponse array>()

            logs |> should not' (be null)
            // サマリーにエンティティ情報とアクションが含まれていることを確認
            logs |> Array.exists (fun l -> l.Summary.Contains("Account") && l.Summary.Contains("TEST001"))
                |> should equal true
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``UPDATE操作の監査ログにOldValuesとNewValuesが含まれる``() : Task =
        task {
            // Arrange - UPDATE操作のログを検索
            let action = "UPDATE"
            let startDate = "2024-01-01T00:00:00"
            let endDate = "2024-01-31T23:59:59"

            // Act
            let! response = client.GetAsync($"/api/v1/audit-logs/action/{action}?startDate={startDate}&endDate={endDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! logs = response.Content.ReadFromJsonAsync<AuditLogResponse array>()

            logs |> should not' (be null)
            logs.Length |> should be (greaterThan 0)

            let updateLog = logs.[0]
            updateLog.OldValues.IsSome |> should equal true
            updateLog.NewValues.IsSome |> should equal true
        }
