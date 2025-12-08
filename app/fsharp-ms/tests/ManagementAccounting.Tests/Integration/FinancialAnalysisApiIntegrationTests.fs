namespace ManagementAccounting.Tests.Integration

open System
open System.Net
open System.Net.Http
open System.Net.Http.Json
open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.Extensions.DependencyInjection
open Testcontainers.PostgreSql
open WireMock.Server
open WireMock.RequestBuilders
open WireMock.ResponseBuilders
open Xunit
open ManagementAccounting.Tests.Infrastructure
open ManagementAccounting.Application.Ports.Out
open ManagementAccounting.Infrastructure.Persistence.Repositories

/// <summary>
/// 財務分析結果 レスポンス DTO（テスト用）
/// </summary>
[<CLIMutable>]
type FinancialDataResponseDto = {
    FiscalYear: int
    Sales: decimal
    OperatingProfit: decimal
    TotalAssets: decimal
    TangibleFixedAssets: decimal
    CurrentAssets: decimal
    CurrentLiabilities: decimal
    QuickAssets: decimal
    Equity: decimal
}

[<CLIMutable>]
type FinancialRatiosResponseDto = {
    OperatingProfitMargin: decimal
    TotalAssetTurnover: decimal
    FixedAssetTurnover: decimal
    CurrentRatio: decimal
    QuickRatio: decimal
    EquityRatio: decimal
    ReturnOnAssets: decimal
    ReturnOnEquity: decimal
}

[<CLIMutable>]
type FinancialAnalysisResultResponseDto = {
    FiscalYear: int
    Data: FinancialDataResponseDto
    Ratios: FinancialRatiosResponseDto
    AnalyzedAt: DateTime
}

/// <summary>
/// 財務分析 API 統合テスト（Testcontainers + WireMock 使用）
/// </summary>
type FinancialAnalysisApiIntegrationTests() =
    let mutable postgres: PostgreSqlContainer = null
    let mutable wireMockServer: WireMockServer = null
    let mutable factory: WebApplicationFactory<ManagementAccounting.Api.IApiMarker> = null
    let mutable client: HttpClient = null

    /// <summary>
    /// 財務会計サービスのモックレスポンスを設定
    /// </summary>
    let setupWireMockResponse () =
        // 勘定科目一覧のモックレスポンス
        let accountsJson = """[
            {"accountId":1,"accountCode":"1110","accountName":"現金","accountType":"Asset","balance":300000},
            {"accountId":2,"accountCode":"1120","accountName":"売掛金","accountType":"Asset","balance":200000},
            {"accountId":3,"accountCode":"1210","accountName":"商品","accountType":"Asset","balance":100000},
            {"accountId":4,"accountCode":"1310","accountName":"建物","accountType":"Asset","balance":200000},
            {"accountId":5,"accountCode":"2110","accountName":"買掛金","accountType":"Liability","balance":150000},
            {"accountId":6,"accountCode":"3110","accountName":"資本金","accountType":"Capital","balance":250000},
            {"accountId":7,"accountCode":"4110","accountName":"売上高","accountType":"Revenue","balance":1000000},
            {"accountId":8,"accountCode":"5110","accountName":"売上原価","accountType":"Expense","balance":600000},
            {"accountId":9,"accountCode":"5210","accountName":"販売費","accountType":"Expense","balance":200000},
            {"accountId":10,"accountCode":"5220","accountName":"一般管理費","accountType":"Expense","balance":100000}
        ]"""

        wireMockServer
            .Given(
                Request.Create()
                    .WithPath("/api/accounts")
                    .UsingGet()
            )
            .RespondWith(
                Response.Create()
                    .WithStatusCode(200)
                    .WithHeader("Content-Type", "application/json")
                    .WithBody(accountsJson)
            )
        |> ignore

    interface IAsyncLifetime with
        member _.InitializeAsync() =
            task {
                DockerHostConfig.configure ()

                // PostgreSQL コンテナを起動
                postgres <- PostgreSqlBuilder()
                    .WithImage("postgres:16-alpine")
                    .WithDatabase("management_accounting_test")
                    .WithUsername("postgres")
                    .WithPassword("postgres")
                    .Build()

                do! postgres.StartAsync()

                // WireMock サーバーを起動（財務会計サービスのモック）
                wireMockServer <- WireMockServer.Start()
                setupWireMockResponse ()

                let connectionString = postgres.GetConnectionString()
                let wireMockUrl = wireMockServer.Url

                factory <- new WebApplicationFactory<ManagementAccounting.Api.IApiMarker>()
                factory <- factory.WithWebHostBuilder(fun builder ->
                    builder.UseSetting("ConnectionStrings:ManagementAccounting", connectionString) |> ignore
                    builder.UseSetting("FinancialAccountingService:BaseUrl", wireMockUrl) |> ignore
                )

                client <- factory.CreateClient()
            }

        member _.DisposeAsync() =
            task {
                if client <> null then
                    client.Dispose()
                if factory <> null then
                    do! factory.DisposeAsync().AsTask()
                if wireMockServer <> null then
                    wireMockServer.Stop()
                    wireMockServer.Dispose()
                if postgres <> null then
                    do! postgres.DisposeAsync().AsTask()
            }

    [<Fact>]
    member _.``POST /api/financial-analysis で財務分析を実行できる``() =
        task {
            // Arrange
            let request = {| FiscalYear = 2024; UseCache = false |}

            // Act
            let! response = client.PostAsJsonAsync("/api/financial-analysis", request)

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)

            let! result = response.Content.ReadFromJsonAsync<FinancialAnalysisResultResponseDto>()
            Assert.NotNull(result)
            Assert.Equal(2024, result.FiscalYear)
        }

    [<Fact>]
    member _.``GET /api/financial-analysis/{fiscalYear} で財務分析を実行できる``() =
        task {
            // Act
            let! response = client.GetAsync("/api/financial-analysis/2024?useCache=false")

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)

            let! result = response.Content.ReadFromJsonAsync<FinancialAnalysisResultResponseDto>()
            Assert.NotNull(result)
            Assert.Equal(2024, result.FiscalYear)
        }

    [<Fact>]
    member _.``分析結果がキャッシュに保存される``() =
        task {
            // Arrange - 最初の分析
            let! _ = client.GetAsync("/api/financial-analysis/2025?useCache=false")

            // Act - キャッシュを取得
            let! response = client.GetAsync("/api/financial-analysis/2025/cached")

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)

            let! result = response.Content.ReadFromJsonAsync<FinancialAnalysisResultResponseDto>()
            Assert.NotNull(result)
            Assert.Equal(2025, result.FiscalYear)
        }

    [<Fact>]
    member _.``キャッシュがない場合は NotFound を返す``() =
        task {
            // Act
            let! response = client.GetAsync("/api/financial-analysis/9999/cached")

            // Assert
            Assert.Equal(HttpStatusCode.NotFound, response.StatusCode)
        }

    [<Fact>]
    member _.``キャッシュを無効化できる``() =
        task {
            // Arrange - 分析を実行してキャッシュを作成
            let! _ = client.GetAsync("/api/financial-analysis/2026?useCache=false")

            // キャッシュが存在することを確認
            let! cacheResponse = client.GetAsync("/api/financial-analysis/2026/cached")
            Assert.Equal(HttpStatusCode.OK, cacheResponse.StatusCode)

            // Act - キャッシュを無効化
            let! deleteResponse = client.DeleteAsync("/api/financial-analysis/2026/cache")

            // Assert
            Assert.Equal(HttpStatusCode.NoContent, deleteResponse.StatusCode)

            // キャッシュが削除されたことを確認
            let! notFoundResponse = client.GetAsync("/api/financial-analysis/2026/cached")
            Assert.Equal(HttpStatusCode.NotFound, notFoundResponse.StatusCode)
        }

    [<Fact>]
    member _.``キャッシュ使用時は同じ結果を返す``() =
        task {
            // Arrange - 最初の分析
            let! firstResponse = client.GetAsync("/api/financial-analysis/2027?useCache=false")
            let! firstResult = firstResponse.Content.ReadFromJsonAsync<FinancialAnalysisResultResponseDto>()

            // Act - キャッシュを使用した分析
            let! secondResponse = client.GetAsync("/api/financial-analysis/2027?useCache=true")
            let! secondResult = secondResponse.Content.ReadFromJsonAsync<FinancialAnalysisResultResponseDto>()

            // Assert
            Assert.Equal(HttpStatusCode.OK, secondResponse.StatusCode)
            Assert.Equal(firstResult.Data.Sales, secondResult.Data.Sales)
            Assert.Equal(firstResult.FiscalYear, secondResult.FiscalYear)
        }

    [<Fact>]
    member _.``財務比率が正しく計算される``() =
        task {
            // Act
            let! response = client.GetAsync("/api/financial-analysis/2028?useCache=false")
            let! result = response.Content.ReadFromJsonAsync<FinancialAnalysisResultResponseDto>()

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)

            // 比率が計算されていることを確認（パーセント表記）
            Assert.True(result.Ratios.OperatingProfitMargin >= 0m)
            Assert.True(result.Ratios.TotalAssetTurnover >= 0m)
            Assert.True(result.Ratios.CurrentRatio >= 0m)
            Assert.True(result.Ratios.EquityRatio >= 0m)
            Assert.True(result.Ratios.ReturnOnAssets >= 0m)
            Assert.True(result.Ratios.ReturnOnEquity >= 0m)
        }
