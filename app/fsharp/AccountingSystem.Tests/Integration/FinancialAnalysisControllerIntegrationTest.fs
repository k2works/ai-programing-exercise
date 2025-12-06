module AccountingSystem.Tests.Integration.FinancialAnalysisControllerIntegrationTest

open System.Net
open System.Net.Http
open System.Net.Http.Json
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open Microsoft.Extensions.DependencyInjection
open AccountingSystem.Infrastructure.Web.Dtos
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services
open AccountingSystem.Infrastructure.Adapters
open AccountingSystem.Infrastructure.Persistence.Repositories.FinancialStatementRepository
open AccountingSystem.Infrastructure.Seed
open AccountingSystem.Infrastructure.Seed.AccountingSeedData
open AccountingSystem.Tests.PostgresWebTestBase
open Npgsql
open Dapper
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Abstractions

/// <summary>
/// 財務分析 API 統合テスト
/// Testcontainers + WebApplicationFactory を使用した E2E テスト
/// D社のシードデータを使用して財務分析機能をテスト
/// </summary>
type FinancialAnalysisControllerIntegrationTest() =
    inherit PostgresWebTestBase()

    /// <summary>
    /// サブクラスで DI 設定をカスタマイズ
    /// </summary>
    override _.ConfigureServices(services: IServiceCollection, connStr: string) =
        // 既存の IFinancialStatementRepository を削除して新しいものに置き換え
        let descriptor =
            services
            |> Seq.tryFind (fun d -> d.ServiceType = typeof<IFinancialStatementRepository>)
        match descriptor with
        | Some d -> services.Remove(d) |> ignore
        | None -> ()

        services.AddScoped<IFinancialStatementRepository>(fun _ ->
            FinancialStatementRepositoryAdapter(connStr) :> IFinancialStatementRepository
        ) |> ignore

        // IFinancialAnalysisUseCase を登録
        services.AddScoped<IFinancialAnalysisUseCase>(fun sp ->
            let repo = sp.GetRequiredService<IFinancialStatementRepository>()
            FinancialAnalysisService(repo) :> IFinancialAnalysisUseCase
        ) |> ignore

    /// <summary>
    /// テストデータのセットアップ
    /// D社のシードデータを投入
    /// </summary>
    override _.SetupTestDataAsync(connStr: string) =
        task {
            let logger = NullLogger<DatabaseSeeder>.Instance
            let seeder = DatabaseSeeder(connStr, logger)
            do! seeder.SeedAsync()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``財務データを取得できる（令和3年度）``() : Task =
        task {
            // Arrange
            let fiscalYear = 2021

            // Act
            let! response = this.Client.GetAsync($"/api/v1/financial-analysis/data/{fiscalYear}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! data = response.Content.ReadFromJsonAsync<FinancialDataResponse>()

            data |> should not' (be null)
            data.FiscalYear |> should equal 2022  // 期末日の年
            data.NetSales |> should equal 5796105m
            data.GrossProfit |> should equal 3610249m
            data.OperatingIncome |> should equal 985027m
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``財務データを取得できる（令和4年度）``() : Task =
        task {
            // Arrange
            let fiscalYear = 2022

            // Act
            let! response = this.Client.GetAsync($"/api/v1/financial-analysis/data/{fiscalYear}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! data = response.Content.ReadFromJsonAsync<FinancialDataResponse>()

            data |> should not' (be null)
            data.FiscalYear |> should equal 2023  // 期末日の年
            data.NetSales |> should equal 4547908m
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``将来の年度でも累積BSデータがあればOKが返る``() : Task =
        task {
            // Arrange
            // シードデータは2021, 2022年度のみだが、FinancialStatementRepositoryは
            // B/S: asOfDate以前のデータを累積で返す → 過去データあり (TotalAssets > 0)
            // P/L: fromDate〜toDate の期間のみ → 2025/4/1〜2026/3/31 にデータなし (TotalRevenues = 0)
            // サービスは TotalAssets = 0 && TotalRevenues = 0 の場合のみエラーなので OK が返る
            let fiscalYear = 2025

            // Act
            let! response = this.Client.GetAsync($"/api/v1/financial-analysis/data/{fiscalYear}")

            // Assert
            // B/S データが累積であるため OK が返る
            response.StatusCode |> should equal HttpStatusCode.OK

            let! data = response.Content.ReadFromJsonAsync<FinancialDataResponse>()
            data |> should not' (be null)
            // B/S は累積なので TotalAssets > 0
            data.TotalAssets |> should be (greaterThan 0m)
            // P/L は期間データなので NetSales = 0
            data.NetSales |> should equal 0m
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``包括的な財務指標を取得できる``() : Task =
        task {
            // Arrange
            let fiscalYear = 2021

            // Act
            let! response = this.Client.GetAsync($"/api/v1/financial-analysis/ratios/{fiscalYear}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! ratios = response.Content.ReadFromJsonAsync<ComprehensiveFinancialRatiosResponse>()

            ratios |> should not' (be null)
            // FiscalYear は期末日の年（2022年3月31日 → 2022）
            ratios.FiscalYear |> should equal 2022
            // 売上高総利益率 = 3610249 / 5796105 * 100 = 62.29%
            ratios.GrossProfitMargin |> should be (greaterThan 62m)
            ratios.GrossProfitMargin |> should be (lessThan 63m)
            // 売上高営業利益率 = 985027 / 5796105 * 100 = 16.99%
            ratios.OperatingProfitMargin |> should be (greaterThan 16m)
            ratios.OperatingProfitMargin |> should be (lessThan 18m)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``複数年度の財務指標を比較できる``() : Task =
        task {
            // Arrange
            let request = { FiscalYears = [| 2021; 2022 |] }
            let content = new StringContent(
                JsonSerializer.Serialize(request),
                Encoding.UTF8,
                "application/json"
            )

            // Act
            let! response = this.Client.PostAsync("/api/v1/financial-analysis/compare", content)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! ratiosList = response.Content.ReadFromJsonAsync<ComprehensiveFinancialRatiosResponse array>()

            ratiosList |> should not' (be null)
            ratiosList.Length |> should equal 2
            // FiscalYear は期末日の年（2021年度 → 2022年3月31日 → 2022）
            ratiosList.[0].FiscalYear |> should equal 2022
            ratiosList.[1].FiscalYear |> should equal 2023
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``空の年度リストで400エラーが返る``() : Task =
        task {
            // Arrange
            let request = { FiscalYears = [||] }
            let content = new StringContent(
                JsonSerializer.Serialize(request),
                Encoding.UTF8,
                "application/json"
            )

            // Act
            let! response = this.Client.PostAsync("/api/v1/financial-analysis/compare", content)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.BadRequest
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``収益性分析を取得できる``() : Task =
        task {
            // Arrange
            let fiscalYear = 2021

            // Act
            let! response = this.Client.GetAsync($"/api/v1/financial-analysis/profitability/{fiscalYear}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! analysis = response.Content.ReadFromJsonAsync<ProfitabilityAnalysisResponse>()

            analysis |> should not' (be null)
            analysis.FiscalYear |> should equal 2021
            analysis.GrossProfitMargin |> should be (greaterThan 60m)
            analysis.OperatingProfitMargin |> should be (greaterThan 15m)
            analysis.Analysis |> should not' (be NullOrEmptyString)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``効率性分析を取得できる``() : Task =
        task {
            // Arrange
            let fiscalYear = 2021

            // Act
            let! response = this.Client.GetAsync($"/api/v1/financial-analysis/efficiency/{fiscalYear}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! analysis = response.Content.ReadFromJsonAsync<EfficiencyAnalysisResponse>()

            analysis |> should not' (be null)
            analysis.FiscalYear |> should equal 2021
            // 総資本回転率 = 5796105 / 2863166 = 2.02
            analysis.TotalAssetTurnover |> should be (greaterThan 2.0m)
            analysis.TotalAssetTurnover |> should be (lessThan 2.1m)
            analysis.Analysis |> should not' (be NullOrEmptyString)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``安全性分析を取得できる``() : Task =
        task {
            // Arrange
            let fiscalYear = 2021

            // Act
            let! response = this.Client.GetAsync($"/api/v1/financial-analysis/safety/{fiscalYear}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! analysis = response.Content.ReadFromJsonAsync<SafetyAnalysisResponse>()

            analysis |> should not' (be null)
            analysis.FiscalYear |> should equal 2021
            // 流動比率 = 2676193 / 851394 * 100 = 314.33%
            analysis.CurrentRatio |> should be (greaterThan 300m)
            // 自己資本比率 = 1989272 / 2863166 * 100 = 69.48%
            analysis.EquityRatio |> should be (greaterThan 69m)
            analysis.EquityRatio |> should be (lessThan 70m)
            analysis.Analysis |> should not' (be NullOrEmptyString)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``収益性分析で将来の年度でも累積データがあればOKが返る``() : Task =
        task {
            // Arrange
            // 将来の年度でも B/S の累積データがあるため OK が返る
            let fiscalYear = 2025

            // Act
            let! response = this.Client.GetAsync($"/api/v1/financial-analysis/profitability/{fiscalYear}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! analysis = response.Content.ReadFromJsonAsync<ProfitabilityAnalysisResponse>()
            analysis |> should not' (be null)
            // P/L データがないため利益率は 0%
            analysis.GrossProfitMargin |> should equal 0m
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``効率性分析で将来の年度でも累積データがあればOKが返る``() : Task =
        task {
            // Arrange
            // 将来の年度でも B/S の累積データがあるため OK が返る
            let fiscalYear = 2025

            // Act
            let! response = this.Client.GetAsync($"/api/v1/financial-analysis/efficiency/{fiscalYear}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! analysis = response.Content.ReadFromJsonAsync<EfficiencyAnalysisResponse>()
            analysis |> should not' (be null)
            // P/L データがないため回転率は 0
            analysis.TotalAssetTurnover |> should equal 0m
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``安全性分析で将来の年度でも累積データがあればOKが返る``() : Task =
        task {
            // Arrange
            // 将来の年度でも B/S の累積データがあるため OK が返る
            let fiscalYear = 2025

            // Act
            let! response = this.Client.GetAsync($"/api/v1/financial-analysis/safety/{fiscalYear}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! analysis = response.Content.ReadFromJsonAsync<SafetyAnalysisResponse>()
            analysis |> should not' (be null)
            // B/S データがあるため安全性指標は計算可能
            analysis.EquityRatio |> should be (greaterThan 0m)
        }
