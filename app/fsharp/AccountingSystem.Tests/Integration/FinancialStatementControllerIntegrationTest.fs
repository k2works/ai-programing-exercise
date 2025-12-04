module AccountingSystem.Tests.Integration.FinancialStatementControllerIntegrationTest

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
open AccountingSystem.Infrastructure.Persistence.Repositories.FinancialStatementRepository
open AccountingSystem.Infrastructure.MigrationRunner
open Npgsql
open Dapper

/// <summary>
/// 財務諸表 API 統合テスト
/// Testcontainers + WebApplicationFactory を使用した E2E テスト
/// </summary>
type FinancialStatementControllerIntegrationTest() =
    let mutable container: PostgreSqlContainer = null
    let mutable connectionString: string = null
    let mutable factory: WebApplicationFactory<Program> = null
    let mutable client: HttpClient = null

    /// <summary>
    /// テスト用の初期データを挿入
    /// </summary>
    let insertTestData (connStr: string) =
        task {
            use connection = new NpgsqlConnection(connStr)
            do! connection.OpenAsync()

            // 勘定科目のテストデータを挿入
            // 資産・負債・純資産・収益・費用をすべて用意
            let accountSql = """
                INSERT INTO "勘定科目マスタ" (
                    "勘定科目コード",
                    "勘定科目名",
                    "勘定科目カナ",
                    "勘定科目種別",
                    "合計科目",
                    "BSPL区分",
                    "取引要素区分",
                    "費用区分",
                    "表示順序",
                    "集計対象",
                    "課税取引コード",
                    "残高"
                )
                VALUES
                    ('1110', '普通預金', 'フツウヨキン', '資産', false, 'B', '1', '', 10, false, NULL, 0),
                    ('1410', '建物', 'タテモノ', '資産', false, 'B', '1', '', 14, false, NULL, 0),
                    ('2110', '買掛金', 'カイカケキン', '負債', false, 'B', '2', '', 20, false, NULL, 0),
                    ('2510', '長期借入金', 'チョウキカリイレキン', '負債', false, 'B', '2', '', 25, false, NULL, 0),
                    ('3110', '資本金', 'シホンキン', '純資産', false, 'B', '3', '', 30, false, NULL, 0),
                    ('4110', '売上高', 'ウリアゲダカ', '収益', false, 'P', '4', '', 40, false, NULL, 0),
                    ('5110', '売上原価', 'ウリアゲゲンカ', '費用', false, 'P', '5', '1', 50, false, NULL, 0),
                    ('6110', '給与手当', 'キュウヨテアテ', '費用', false, 'P', '5', '2', 60, false, NULL, 0),
                    ('6120', '法定福利費', 'ホウテイフクリヒ', '費用', false, 'P', '5', '2', 61, false, NULL, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING;
            """

            let! _ = connection.ExecuteAsync(accountSql)

            // 日次勘定科目残高のテストデータを挿入
            // これが財務諸表の元になるデータ
            let balanceSql = """
                INSERT INTO "日次勘定科目残高" (
                    "勘定科目コード",
                    "起票日",
                    "借方金額",
                    "貸方金額"
                )
                VALUES
                    -- 資産（借方増加）
                    ('1110', '2024-01-15', 8000000, 0),
                    ('1410', '2024-01-15', 2000000, 0),
                    -- 負債（貸方増加）
                    ('2110', '2024-01-15', 0, 500000),
                    ('2510', '2024-01-15', 0, 4500000),
                    -- 純資産（貸方増加）
                    ('3110', '2024-01-15', 0, 5000000),
                    -- 収益（貸方増加）
                    ('4110', '2024-01-15', 0, 10000000),
                    -- 費用（借方増加）
                    ('5110', '2024-01-15', 6000000, 0),
                    ('6110', '2024-01-15', 2000000, 0),
                    ('6120', '2024-01-15', 500000, 0)
                ON CONFLICT DO NOTHING;
            """

            let! _ = connection.ExecuteAsync(balanceSql)
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
                                // 既存の IFinancialStatementRepository を削除して新しいものに置き換え
                                let descriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<IFinancialStatementRepository>)
                                match descriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                services.AddScoped<IFinancialStatementRepository>(fun _ ->
                                    FinancialStatementRepositoryAdapter(connectionString) :> IFinancialStatementRepository
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
    member _.``貸借対照表を取得できる``() : Task =
        task {
            // Arrange
            let asOfDate = "2024-01-31"

            // Act
            let! response = client.GetAsync($"/api/v1/financial-statements/balance-sheet?asOfDate={asOfDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! balanceSheet = response.Content.ReadFromJsonAsync<BalanceSheetResponse>()

            balanceSheet |> should not' (be null)
            balanceSheet.TotalAssets |> should equal 10000000M
            balanceSheet.TotalLiabilities |> should equal 5000000M
            balanceSheet.TotalEquity |> should equal 5000000M
            balanceSheet.TotalLiabilitiesAndEquity |> should equal 10000000M
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``貸借対照表の資産項目を取得できる``() : Task =
        task {
            // Arrange
            let asOfDate = "2024-01-31"

            // Act
            let! response = client.GetAsync($"/api/v1/financial-statements/balance-sheet?asOfDate={asOfDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! balanceSheet = response.Content.ReadFromJsonAsync<BalanceSheetResponse>()

            balanceSheet.Assets |> should not' (be Empty)
            balanceSheet.Assets |> List.exists (fun a -> a.AccountCode = "1110" && a.Balance = 8000000M) |> should be True
            balanceSheet.Assets |> List.exists (fun a -> a.AccountCode = "1410" && a.Balance = 2000000M) |> should be True
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``損益計算書を取得できる``() : Task =
        task {
            // Arrange
            let fromDate = "2024-01-01"
            let toDate = "2024-01-31"

            // Act
            let! response = client.GetAsync($"/api/v1/financial-statements/income-statement?fromDate={fromDate}&toDate={toDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! incomeStatement = response.Content.ReadFromJsonAsync<IncomeStatementResponse>()

            incomeStatement |> should not' (be null)
            incomeStatement.TotalRevenues |> should equal 10000000M
            incomeStatement.TotalExpenses |> should equal 8500000M
            incomeStatement.NetIncome |> should equal 1500000M
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``損益計算書の利益項目を取得できる``() : Task =
        task {
            // Arrange
            let fromDate = "2024-01-01"
            let toDate = "2024-01-31"

            // Act
            let! response = client.GetAsync($"/api/v1/financial-statements/income-statement?fromDate={fromDate}&toDate={toDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! incomeStatement = response.Content.ReadFromJsonAsync<IncomeStatementResponse>()

            // 売上総利益 = 売上高 10,000,000 - 売上原価 6,000,000 = 4,000,000
            incomeStatement.GrossProfit |> should equal 4000000M
            // 営業利益 = 売上総利益 4,000,000 - 販管費 2,500,000 = 1,500,000
            incomeStatement.OperatingIncome |> should equal 1500000M
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``財務指標を取得できる``() : Task =
        task {
            // Arrange
            let asOfDate = "2024-01-31"
            let fromDate = "2024-01-01"
            let toDate = "2024-01-31"

            // Act
            let! response = client.GetAsync($"/api/v1/financial-statements/ratios?asOfDate={asOfDate}&fromDate={fromDate}&toDate={toDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! ratios = response.Content.ReadFromJsonAsync<FinancialRatiosResponse>()

            ratios |> should not' (be null)
            // 自己資本比率 = 5,000,000 / 10,000,000 × 100 = 50%
            ratios.EquityRatio |> should equal 50.00M
            // 売上総利益率 = 4,000,000 / 10,000,000 × 100 = 40%
            ratios.GrossProfitMargin |> should equal 40.00M
            // 営業利益率 = 1,500,000 / 10,000,000 × 100 = 15%
            ratios.OperatingProfitMargin |> should equal 15.00M
            // 当期純利益率 = 1,500,000 / 10,000,000 × 100 = 15%
            ratios.NetProfitMargin |> should equal 15.00M
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``財務指標のROA・ROEを取得できる``() : Task =
        task {
            // Arrange
            let asOfDate = "2024-01-31"
            let fromDate = "2024-01-01"
            let toDate = "2024-01-31"

            // Act
            let! response = client.GetAsync($"/api/v1/financial-statements/ratios?asOfDate={asOfDate}&fromDate={fromDate}&toDate={toDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! ratios = response.Content.ReadFromJsonAsync<FinancialRatiosResponse>()

            // ROA = 1,500,000 / 10,000,000 × 100 = 15%
            ratios.Roa |> should equal 15.00M
            // ROE = 1,500,000 / 5,000,000 × 100 = 30%
            ratios.Roe |> should equal 30.00M
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``該当データがない日付で空の貸借対照表が返る``() : Task =
        task {
            // Arrange - テストデータより前の日付
            let asOfDate = "2023-01-01"

            // Act
            let! response = client.GetAsync($"/api/v1/financial-statements/balance-sheet?asOfDate={asOfDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! balanceSheet = response.Content.ReadFromJsonAsync<BalanceSheetResponse>()

            balanceSheet |> should not' (be null)
            balanceSheet.TotalAssets |> should equal 0M
            balanceSheet.TotalLiabilities |> should equal 0M
            balanceSheet.TotalEquity |> should equal 0M
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``該当データがない日付範囲で空の損益計算書が返る``() : Task =
        task {
            // Arrange - テストデータより前の日付範囲
            let fromDate = "2023-01-01"
            let toDate = "2023-01-31"

            // Act
            let! response = client.GetAsync($"/api/v1/financial-statements/income-statement?fromDate={fromDate}&toDate={toDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! incomeStatement = response.Content.ReadFromJsonAsync<IncomeStatementResponse>()

            incomeStatement |> should not' (be null)
            incomeStatement.TotalRevenues |> should equal 0M
            incomeStatement.TotalExpenses |> should equal 0M
            incomeStatement.NetIncome |> should equal 0M
        }
