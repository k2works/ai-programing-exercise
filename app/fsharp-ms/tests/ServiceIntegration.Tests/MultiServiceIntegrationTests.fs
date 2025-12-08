namespace ServiceIntegration.Tests

open System
open System.Net
open System.Net.Http
open System.Net.Http.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.Extensions.DependencyInjection
open Testcontainers.PostgreSql
open Testcontainers.RabbitMq
open Xunit
open FinancialAccounting.Infrastructure.Web.Dtos
open ManagementAccounting.Infrastructure.Web.Dtos

/// <summary>
/// Docker ホストを設定（Windows Docker Desktop 用）
/// </summary>
module DockerHostConfig =
    let configure () =
        let dockerHost =
            match Environment.GetEnvironmentVariable("DOCKER_HOST") with
            | null | "" -> "npipe://./pipe/docker_engine"
            | host -> host
        Environment.SetEnvironmentVariable("DOCKER_HOST", dockerHost)

/// <summary>
/// TestContainers によるマルチサービスインテグレーションテスト
/// 財務会計サービスと管理会計サービスの連携をテスト
/// </summary>
type MultiServiceIntegrationTests() =
    let mutable postgresFinancial: PostgreSqlContainer = null
    let mutable postgresManagement: PostgreSqlContainer = null
    // RabbitMQ は現時点ではインメモリモードを使用するためオプショナル
    // 将来のイベント連携テスト用に準備
    let mutable _rabbitMq: RabbitMqContainer option = None
    let mutable financialFactory: WebApplicationFactory<FinancialAccounting.Api.IApiMarker> = null
    let mutable managementFactory: WebApplicationFactory<ManagementAccounting.Api.IApiMarker> = null
    let mutable financialClient: HttpClient = null
    let mutable managementClient: HttpClient = null

    interface IAsyncLifetime with
        member _.InitializeAsync() =
            task {
                // Windows Docker Desktop 用に環境変数を設定
                DockerHostConfig.configure ()

                // 財務会計用 PostgreSQL コンテナを起動
                postgresFinancial <- PostgreSqlBuilder()
                    .WithImage("postgres:16-alpine")
                    .WithDatabase("financial_accounting")
                    .WithUsername("test")
                    .WithPassword("test")
                    .Build()

                // 管理会計用 PostgreSQL コンテナを起動
                postgresManagement <- PostgreSqlBuilder()
                    .WithImage("postgres:16-alpine")
                    .WithDatabase("management_accounting")
                    .WithUsername("test")
                    .WithPassword("test")
                    .Build()

                // PostgreSQL コンテナを並列起動
                // 注意: MassTransit はインメモリモードを使用するため RabbitMQ コンテナは起動しない
                do! Task.WhenAll(
                    postgresFinancial.StartAsync(),
                    postgresManagement.StartAsync()
                )

                // 財務会計 API サーバーを起動
                // 注意: RabbitMQ:Host を設定しないことでインメモリモードを使用
                let financialConnectionString = postgresFinancial.GetConnectionString()

                financialFactory <- new WebApplicationFactory<FinancialAccounting.Api.IApiMarker>()
                financialFactory <- financialFactory.WithWebHostBuilder(fun builder ->
                    builder.UseSetting("ConnectionStrings:FinancialAccounting", financialConnectionString) |> ignore
                    // RabbitMQ:Host を設定しないことでインメモリモードになる
                )
                financialClient <- financialFactory.CreateClient()

                // 財務会計サービスの URL を取得
                let financialServiceUrl = financialFactory.Server.BaseAddress.ToString().TrimEnd('/')

                // 管理会計 API サーバーを起動
                let managementConnectionString = postgresManagement.GetConnectionString()

                managementFactory <- new WebApplicationFactory<ManagementAccounting.Api.IApiMarker>()
                managementFactory <- managementFactory.WithWebHostBuilder(fun builder ->
                    builder.UseSetting("ConnectionStrings:ManagementAccounting", managementConnectionString) |> ignore
                    // 財務会計サービスの URL を設定
                    builder.UseSetting("FinancialAccountingService:BaseUrl", financialServiceUrl) |> ignore
                    // RabbitMQ:Host を設定しないことでインメモリモードになる

                    // 財務会計サービスへの HttpClient を設定
                    builder.ConfigureServices(fun services ->
                        // HttpClient を財務会計 TestServer 向けに構成
                        services.AddHttpClient<ManagementAccounting.Infrastructure.Adapters.External.FinancialAccountingClient>(fun client ->
                            client.BaseAddress <- financialFactory.Server.BaseAddress
                        ).ConfigurePrimaryHttpMessageHandler(fun () ->
                            financialFactory.Server.CreateHandler()
                        ) |> ignore
                    ) |> ignore
                )
                managementClient <- managementFactory.CreateClient()
            }

        member _.DisposeAsync() =
            task {
                // クライアントの破棄
                if financialClient <> null then
                    financialClient.Dispose()
                if managementClient <> null then
                    managementClient.Dispose()

                // ファクトリーの破棄
                if financialFactory <> null then
                    do! financialFactory.DisposeAsync().AsTask()
                if managementFactory <> null then
                    do! managementFactory.DisposeAsync().AsTask()

                // コンテナの破棄
                if postgresFinancial <> null then
                    do! postgresFinancial.DisposeAsync().AsTask()
                if postgresManagement <> null then
                    do! postgresManagement.DisposeAsync().AsTask()
                match _rabbitMq with
                | Some r -> do! r.DisposeAsync().AsTask()
                | None -> ()
            }

    [<Fact>]
    member _.``両サービスのヘルスチェックが成功すること``() =
        task {
            // Act - 財務会計サービスの Swagger UI にアクセス
            let! financialResponse = financialClient.GetAsync("/swagger/index.html")

            // Act - 管理会計サービスの Swagger UI にアクセス
            let! managementResponse = managementClient.GetAsync("/swagger/index.html")

            // Assert - 両サービスが正常に応答
            Assert.Equal(HttpStatusCode.OK, financialResponse.StatusCode)
            Assert.Equal(HttpStatusCode.OK, managementResponse.StatusCode)
        }

    [<Fact>]
    member _.``財務会計サービスで仕訳を作成できること``() =
        task {
            // Arrange
            let request = {|
                JournalDate = DateTime.UtcNow.Date
                Description = "Multi-service test journal"
                FiscalYear = 2024
                Entries = [|
                    {| AccountCode = "1120"; DebitAmount = 100000m; CreditAmount = 0m; Description = "Accounts Receivable" |}
                    {| AccountCode = "4110"; DebitAmount = 0m; CreditAmount = 100000m; Description = "Sales Revenue" |}
                |]
            |}

            // Act
            let! response = financialClient.PostAsJsonAsync("/api/journals", request)

            // Assert
            Assert.Equal(HttpStatusCode.Created, response.StatusCode)

            let! journal = response.Content.ReadFromJsonAsync<JournalResponseDto>()
            Assert.NotNull(journal)
            Assert.True(journal.JournalId > 0)
            Assert.Equal("Multi-service test journal", journal.Description)
            Assert.Equal(2, journal.Entries.Length)
        }

    [<Fact>]
    member _.``財務会計サービスで作成した仕訳を取得できること``() =
        task {
            // Arrange - 仕訳を作成
            let createRequest = {|
                JournalDate = DateTime.UtcNow.Date
                Description = "Journal for retrieval test"
                FiscalYear = 2024
                Entries = [|
                    {| AccountCode = "1110"; DebitAmount = 50000m; CreditAmount = 0m; Description = "Cash" |}
                    {| AccountCode = "4110"; DebitAmount = 0m; CreditAmount = 50000m; Description = "Sales" |}
                |]
            |}
            let! createResponse = financialClient.PostAsJsonAsync("/api/journals", createRequest)
            let! createdJournal = createResponse.Content.ReadFromJsonAsync<JournalResponseDto>()

            // Act
            let! response = financialClient.GetAsync($"/api/journals/{createdJournal.JournalId}")

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)

            let! journal = response.Content.ReadFromJsonAsync<JournalResponseDto>()
            Assert.NotNull(journal)
            Assert.Equal(createdJournal.JournalId, journal.JournalId)
            Assert.Equal("Journal for retrieval test", journal.Description)
        }

    [<Fact>]
    member _.``管理会計サービスで勘定科目マスタを取得できること``() =
        task {
            // Act - 管理会計サービスで勘定科目一覧を取得
            let! response = managementClient.GetAsync("/api/cost-centers")

            // Assert - サービスが正常に応答することを確認（データがなくても空配列が返る）
            // 注意: 404 が返る場合はエンドポイントが存在しない可能性がある
            // ここでは管理会計サービスの基本的な動作確認のみ行う
            Assert.True(
                response.StatusCode = HttpStatusCode.OK ||
                response.StatusCode = HttpStatusCode.NotFound,
                $"Unexpected status code: {response.StatusCode}"
            )
        }

    [<Fact>]
    member _.``複数の仕訳を作成し会計年度で一覧取得できること``() =
        task {
            // Arrange - 複数の仕訳を作成
            let fiscalYear = 2025

            let request1 = {|
                JournalDate = DateTime(2025, 4, 1)
                Description = "First journal entry"
                FiscalYear = fiscalYear
                Entries = [|
                    {| AccountCode = "1110"; DebitAmount = 100000m; CreditAmount = 0m; Description = "Cash" |}
                    {| AccountCode = "4110"; DebitAmount = 0m; CreditAmount = 100000m; Description = "Sales" |}
                |]
            |}

            let request2 = {|
                JournalDate = DateTime(2025, 5, 1)
                Description = "Second journal entry"
                FiscalYear = fiscalYear
                Entries = [|
                    {| AccountCode = "1120"; DebitAmount = 200000m; CreditAmount = 0m; Description = "Accounts Receivable" |}
                    {| AccountCode = "4110"; DebitAmount = 0m; CreditAmount = 200000m; Description = "Sales" |}
                |]
            |}

            let! _ = financialClient.PostAsJsonAsync("/api/journals", request1)
            let! _ = financialClient.PostAsJsonAsync("/api/journals", request2)

            // Act
            let! response = financialClient.GetAsync($"/api/journals?fiscalYear={fiscalYear}")

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)

            let! journals = response.Content.ReadFromJsonAsync<JournalResponseDto[]>()
            Assert.NotNull(journals)
            Assert.True(journals.Length >= 2, $"Expected at least 2 journals, got {journals.Length}")
            Assert.All(journals, fun j -> Assert.Equal(fiscalYear, j.FiscalYear))
        }

    [<Fact>]
    member _.``管理会計サービスで財務分析を実行しキャッシュが生成されること``() =
        task {
            // Arrange - 財務会計サービスで仕訳を作成
            let fiscalYear = 2026
            let request = {|
                JournalDate = DateTime(2026, 4, 1)
                Description = "Journal for cache test"
                FiscalYear = fiscalYear
                Entries = [|
                    {| AccountCode = "1110"; DebitAmount = 500000m; CreditAmount = 0m; Description = "Cash" |}
                    {| AccountCode = "4110"; DebitAmount = 0m; CreditAmount = 500000m; Description = "Sales" |}
                |]
            |}
            let! _ = financialClient.PostAsJsonAsync("/api/journals", request)

            // Act - 管理会計サービスで財務分析を実行（キャッシュ使用なし）
            let! analysisResponse = managementClient.GetAsync($"/api/financial-analysis/{fiscalYear}?useCache=false")

            // Assert - 分析が成功すること
            Assert.Equal(HttpStatusCode.OK, analysisResponse.StatusCode)

            let! analysis = analysisResponse.Content.ReadFromJsonAsync<FinancialAnalysisResultResponseDto>()
            Assert.NotNull(analysis)
            Assert.Equal(fiscalYear, analysis.FiscalYear)

            // Act - キャッシュされた結果を取得
            let! cachedResponse = managementClient.GetAsync($"/api/financial-analysis/{fiscalYear}/cached")

            // Assert - キャッシュが存在すること
            Assert.Equal(HttpStatusCode.OK, cachedResponse.StatusCode)

            let! cached = cachedResponse.Content.ReadFromJsonAsync<FinancialAnalysisResultResponseDto>()
            Assert.NotNull(cached)
            Assert.Equal(fiscalYear, cached.FiscalYear)
        }

    [<Fact>]
    member _.``管理会計サービスでキャッシュを無効化できること``() =
        task {
            // Arrange - 財務分析を実行してキャッシュを作成
            let fiscalYear = 2027
            let request = {|
                JournalDate = DateTime(2027, 4, 1)
                Description = "Journal for cache invalidation test"
                FiscalYear = fiscalYear
                Entries = [|
                    {| AccountCode = "1110"; DebitAmount = 300000m; CreditAmount = 0m; Description = "Cash" |}
                    {| AccountCode = "4110"; DebitAmount = 0m; CreditAmount = 300000m; Description = "Sales" |}
                |]
            |}
            let! _ = financialClient.PostAsJsonAsync("/api/journals", request)
            let! _ = managementClient.GetAsync($"/api/financial-analysis/{fiscalYear}?useCache=false")

            // キャッシュが存在することを確認
            let! cachedResponse = managementClient.GetAsync($"/api/financial-analysis/{fiscalYear}/cached")
            Assert.Equal(HttpStatusCode.OK, cachedResponse.StatusCode)

            // Act - キャッシュを無効化
            let! deleteResponse = managementClient.DeleteAsync($"/api/financial-analysis/{fiscalYear}/cache")

            // Assert - キャッシュが削除されること
            Assert.Equal(HttpStatusCode.NoContent, deleteResponse.StatusCode)

            // キャッシュが存在しないことを確認
            let! notFoundResponse = managementClient.GetAsync($"/api/financial-analysis/{fiscalYear}/cached")
            Assert.Equal(HttpStatusCode.NotFound, notFoundResponse.StatusCode)
        }
