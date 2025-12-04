module AccountingSystem.Tests.Integration.JournalControllerIntegrationTest

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
/// 仕訳 API 統合テスト
/// Testcontainers + WebApplicationFactory を使用した E2E テスト
/// </summary>
type JournalControllerIntegrationTest() =
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
                    ('2110', '買掛金', 'カイカケキン', '負債', false, 'B', '2', '', 20, false, NULL, 0),
                    ('4110', '売上高', 'ウリアゲダカ', '収益', false, 'P', '4', '', 40, false, NULL, 0),
                    ('5110', '売上原価', 'ウリアゲゲンカ', '費用', false, 'P', '5', '', 50, false, NULL, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING;
            """

            let! _ = connection.ExecuteAsync(accountSql)

            // 仕訳のテストデータを挿入（3層構造）
            let journalSql = """
                INSERT INTO "仕訳" (
                    "仕訳伝票番号",
                    "起票日",
                    "入力日",
                    "決算仕訳フラグ",
                    "単振フラグ",
                    "仕訳伝票区分",
                    "定期計上フラグ",
                    "社員コード",
                    "部門コード",
                    "赤伝フラグ",
                    "赤黒伝票番号"
                )
                VALUES
                    ('1001', '2024-01-15', '2024-01-15', 0, 0, 3, 0, NULL, NULL, 0, NULL),
                    ('1002', '2024-01-20', '2024-01-20', 0, 0, 3, 0, NULL, NULL, 0, NULL)
                ON CONFLICT ("仕訳伝票番号") DO NOTHING;
            """

            let! _ = connection.ExecuteAsync(journalSql)

            // 仕訳明細のテストデータを挿入
            let lineSql = """
                INSERT INTO "仕訳明細" (
                    "仕訳伝票番号",
                    "仕訳行番号",
                    "行摘要"
                )
                VALUES
                    ('1001', 1, '売上計上'),
                    ('1002', 1, '仕入計上')
                ON CONFLICT ("仕訳伝票番号", "仕訳行番号") DO NOTHING;
            """

            let! _ = connection.ExecuteAsync(lineSql)

            // 仕訳貸借明細のテストデータを挿入
            let itemSql = """
                INSERT INTO "仕訳貸借明細" (
                    "仕訳伝票番号",
                    "仕訳行番号",
                    "仕訳行貸借区分",
                    "通貨コード",
                    "為替レート",
                    "部門コード",
                    "プロジェクトコード",
                    "勘定科目コード",
                    "補助科目コード",
                    "仕訳金額",
                    "基軸換算仕訳金額",
                    "消費税区分",
                    "消費税率",
                    "消費税計算区分",
                    "期日",
                    "資金繰フラグ",
                    "セグメントコード",
                    "相手勘定科目コード",
                    "相手補助科目コード",
                    "付箋コード",
                    "付箋内容"
                )
                VALUES
                    ('1001', 1, 'D', 'JPY', 1.0, NULL, NULL, '1110', NULL, 100000, 100000, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL),
                    ('1001', 1, 'C', 'JPY', 1.0, NULL, NULL, '4110', NULL, 100000, 100000, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL),
                    ('1002', 1, 'D', 'JPY', 1.0, NULL, NULL, '5110', NULL, 50000, 50000, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL),
                    ('1002', 1, 'C', 'JPY', 1.0, NULL, NULL, '2110', NULL, 50000, 50000, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL)
                ON CONFLICT DO NOTHING;
            """

            let! _ = connection.ExecuteAsync(itemSql)
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
                                // 既存の IJournalRepository を削除して新しいものに置き換え
                                let descriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<IJournalRepository>)
                                match descriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                services.AddScoped<IJournalRepository>(fun _ ->
                                    JournalRepositoryAdapter(connectionString) :> IJournalRepository
                                ) |> ignore

                                // IAccountRepository も置き換え（仕訳作成時に必要）
                                let accountDescriptor =
                                    services
                                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<IAccountRepository>)
                                match accountDescriptor with
                                | Some d -> services.Remove(d) |> ignore
                                | None -> ()

                                services.AddScoped<IAccountRepository>(fun _ ->
                                    AccountRepositoryAdapter(connectionString) :> IAccountRepository
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
    member _.``伝票番号で仕訳を取得できる``() : Task =
        task {
            // Arrange
            let voucherNumber = 1001

            // Act
            let! response = client.GetAsync($"/api/v1/journals/{voucherNumber}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! journal = response.Content.ReadFromJsonAsync<JournalResponse>()

            journal |> should not' (be null)
            journal.VoucherNumber |> should equal "1001"
            journal.Lines |> should not' (be Empty)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``存在しない伝票番号で404が返る``() : Task =
        task {
            // Arrange
            let voucherNumber = 9999

            // Act
            let! response = client.GetAsync($"/api/v1/journals/{voucherNumber}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.NotFound
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``日付範囲で仕訳を取得できる``() : Task =
        task {
            // Arrange
            let fromDate = "2024-01-01"
            let toDate = "2024-01-31"

            // Act
            let! response = client.GetAsync($"/api/v1/journals?fromDate={fromDate}&toDate={toDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! journals = response.Content.ReadFromJsonAsync<JournalResponse array>()

            journals |> should not' (be null)
            journals.Length |> should be (greaterThan 0)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``該当する仕訳がない日付範囲で空リストが返る``() : Task =
        task {
            // Arrange
            let fromDate = "2025-01-01"
            let toDate = "2025-01-31"

            // Act
            let! response = client.GetAsync($"/api/v1/journals?fromDate={fromDate}&toDate={toDate}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! journals = response.Content.ReadFromJsonAsync<JournalResponse array>()

            journals |> should not' (be null)
            journals.Length |> should equal 0
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``新しい仕訳を作成できる``() : Task =
        task {
            // Arrange
            let request : JournalRequest = {
                VoucherNumber = 2001
                PostingDate = DateTime(2024, 2, 1)
                EntryDate = DateTime(2024, 2, 1)
                SettlementFlag = 0
                IsSingleEntry = false
                VoucherType = "TRANSFER"
                IsRecurring = false
                EmployeeCode = ""
                DepartmentCode = ""
                RedSlipFlag = 0
                RedBlackVoucherNumber = Nullable()
                Lines = [|
                    {
                        LineNumber = 1
                        Description = "テスト仕訳"
                        Items = [|
                            {
                                DebitCreditType = "D"
                                AccountCode = "1110"
                                SubAccountCode = ""
                                DepartmentCode = ""
                                ProjectCode = ""
                                Amount = 50000M
                                CurrencyCode = "JPY"
                                ExchangeRate = 1.0M
                                TaxCategory = ""
                                TaxRate = Nullable()
                                TaxCalculationType = ""
                                DueDate = Nullable()
                                IsCashFlow = false
                                SegmentCode = ""
                                CounterAccountCode = ""
                                CounterSubAccountCode = ""
                                MemoCode = ""
                                MemoContent = ""
                            }
                            {
                                DebitCreditType = "C"
                                AccountCode = "4110"
                                SubAccountCode = ""
                                DepartmentCode = ""
                                ProjectCode = ""
                                Amount = 50000M
                                CurrencyCode = "JPY"
                                ExchangeRate = 1.0M
                                TaxCategory = ""
                                TaxRate = Nullable()
                                TaxCalculationType = ""
                                DueDate = Nullable()
                                IsCashFlow = false
                                SegmentCode = ""
                                CounterAccountCode = ""
                                CounterSubAccountCode = ""
                                MemoCode = ""
                                MemoContent = ""
                            }
                        |]
                    }
                |]
            }

            // Act
            let! response = client.PostAsJsonAsync("/api/v1/journals", request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.Created

            let! created = response.Content.ReadFromJsonAsync<JournalResponse>()

            created |> should not' (be null)
            created.VoucherNumber |> should equal "2001"
            created.TotalDebit |> should equal 50000M
            created.TotalCredit |> should equal 50000M
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member _.``仕訳を削除できる``() : Task =
        task {
            // Arrange - 最初に削除対象が存在することを確認
            let voucherNumber = 1002
            let! getResponse = client.GetAsync($"/api/v1/journals/{voucherNumber}")
            getResponse.StatusCode |> should equal HttpStatusCode.OK

            // Act
            let! deleteResponse = client.DeleteAsync($"/api/v1/journals/{voucherNumber}")

            // Assert
            deleteResponse.StatusCode |> should equal HttpStatusCode.NoContent

            // 削除後に取得を試みると 404 になることを確認
            let! afterDeleteResponse = client.GetAsync($"/api/v1/journals/{voucherNumber}")
            afterDeleteResponse.StatusCode |> should equal HttpStatusCode.NotFound
        }
