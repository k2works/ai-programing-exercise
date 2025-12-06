module AccountingSystem.Tests.Integration.AccountControllerIntegrationTest

open System
open System.Net
open System.Net.Http.Json
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open Microsoft.Extensions.DependencyInjection
open AccountingSystem.Infrastructure.Web.Dtos
open AccountingSystem.Application.Port.Out
open AccountingSystem.Infrastructure.Adapters
open AccountingSystem.Tests.PostgresWebTestBase
open Npgsql
open Dapper

/// <summary>
/// 勘定科目 API 統合テスト
/// Testcontainers + WebApplicationFactory を使用した E2E テスト
/// PostgresWebTestBase を継承してコンテナとWebファクトリを管理
/// </summary>
type AccountControllerIntegrationTest() =
    inherit PostgresWebTestBase()

    /// <summary>
    /// サブクラスで DI 設定をカスタマイズ
    /// </summary>
    override _.ConfigureServices(services: IServiceCollection, connStr: string) =
        // 既存の IAccountRepository を削除して新しいものに置き換え
        let descriptor =
            services
            |> Seq.tryFind (fun d -> d.ServiceType = typeof<IAccountRepository>)
        match descriptor with
        | Some d -> services.Remove(d) |> ignore
        | None -> ()

        services.AddScoped<IAccountRepository>(fun _ ->
            AccountRepositoryAdapter(connStr) :> IAccountRepository
        ) |> ignore

    /// <summary>
    /// テストデータのセットアップ
    /// </summary>
    override _.SetupTestDataAsync(connStr: string) =
        task {
            use connection = new NpgsqlConnection(connStr)
            do! connection.OpenAsync()

            // テストデータの挿入（日本語テーブル名・カラム名を使用）
            // 課税取引コードは外部キー制約があるため NULL を使用
            let sql = """
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
                    ('3110', '資本金', 'シホンキン', '純資産', false, 'B', '3', '', 30, false, NULL, 0),
                    ('4110', '売上高', 'ウリアゲダカ', '収益', false, 'P', '4', '', 40, false, NULL, 0),
                    ('5110', '売上原価', 'ウリアゲゲンカ', '費用', false, 'P', '5', '', 50, false, NULL, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING;
            """

            let! _ = connection.ExecuteAsync(sql)
            return ()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``勘定科目一覧を取得できる``() : Task =
        task {
            // Arrange & Act
            let! response = this.Client.GetAsync("/api/v1/accounts")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! accounts = response.Content.ReadFromJsonAsync<AccountResponse array>()

            accounts |> should not' (be null)
            accounts.Length |> should be (greaterThan 0)
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``勘定科目コードで検索できる``() : Task =
        task {
            // Arrange
            let accountCode = "1110"

            // Act
            let! response = this.Client.GetAsync($"/api/v1/accounts/{accountCode}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! account = response.Content.ReadFromJsonAsync<AccountResponse>()

            account |> should not' (be null)
            account.AccountCode |> should equal "1110"
            account.AccountName |> should equal "普通預金"
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``存在しない勘定科目コードで404が返る``() : Task =
        task {
            // Arrange
            let accountCode = "9999"

            // Act
            let! response = this.Client.GetAsync($"/api/v1/accounts/{accountCode}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.NotFound
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``勘定科目を作成できる``() : Task =
        task {
            // Arrange
            let request : AccountRequest = {
                AccountCode = "8888"
                AccountName = "テスト勘定科目"
                AccountNameKana = "テストカンジョウカモク"
                AccountType = "資産"
                IsSummaryAccount = false
                BsplType = "B"
                TransactionElementType = "1"
                ExpenseType = ""
                DisplayOrder = 100
                IsAggregationTarget = false
                TaxCode = ""
            }

            // Act
            let! response = this.Client.PostAsJsonAsync("/api/v1/accounts", request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.Created

            let! created = response.Content.ReadFromJsonAsync<AccountResponse>()

            created |> should not' (be null)
            created.AccountCode |> should equal "8888"
            created.AccountName |> should equal "テスト勘定科目"
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``重複する勘定科目コードで409が返る``() : Task =
        task {
            // Arrange - 既存の勘定科目コードを使用
            let request : AccountRequest = {
                AccountCode = "1110"  // 既存
                AccountName = "重複テスト"
                AccountNameKana = ""
                AccountType = "資産"
                IsSummaryAccount = false
                BsplType = "B"
                TransactionElementType = "1"
                ExpenseType = ""
                DisplayOrder = 100
                IsAggregationTarget = false
                TaxCode = ""
            }

            // Act
            let! response = this.Client.PostAsJsonAsync("/api/v1/accounts", request)

            // Assert
            response.StatusCode |> should equal HttpStatusCode.Conflict
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``勘定科目種別で検索できる``() : Task =
        task {
            // Arrange
            let accountType = "資産"

            // Act - /api/v1/accounts/type/{accountType} を使用
            let! response = this.Client.GetAsync($"/api/v1/accounts/type/{Uri.EscapeDataString(accountType)}")

            // Assert
            response.StatusCode |> should equal HttpStatusCode.OK

            let! accounts = response.Content.ReadFromJsonAsync<AccountResponse array>()

            accounts |> should not' (be null)
            accounts |> Array.iter (fun a ->
                a.AccountType |> should equal "資産"
            )
        }
