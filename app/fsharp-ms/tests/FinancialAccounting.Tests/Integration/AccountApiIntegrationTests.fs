namespace FinancialAccounting.Tests.Integration

open System
open System.Net
open System.Net.Http
open System.Net.Http.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc.Testing
open Testcontainers.PostgreSql
open Xunit
open FinancialAccounting.Tests.Infrastructure

/// <summary>
/// 勘定科目レスポンス DTO（テスト用）
/// </summary>
[<CLIMutable>]
type AccountResponseDto = {
    AccountId: int
    AccountCode: string
    AccountName: string
    AccountNameKana: string
    AccountType: string
    IsSummaryAccount: bool
    BsPlType: string
    TransactionElementType: string
    ExpenseType: string
    DisplayOrder: int
    IsAggregationTarget: bool
    TaxCode: string
    Balance: decimal
    CreatedAt: DateTime
    UpdatedAt: DateTime
}

/// <summary>
/// 勘定科目 API 統合テスト（Testcontainers 使用）
/// </summary>
type AccountApiIntegrationTests() =
    let mutable postgres: PostgreSqlContainer = null
    let mutable factory: WebApplicationFactory<FinancialAccounting.Api.IApiMarker> = null
    let mutable client: HttpClient = null

    interface IAsyncLifetime with
        member _.InitializeAsync() =
            task {
                DockerHostConfig.configure ()

                postgres <- PostgreSqlBuilder()
                    .WithImage("postgres:16-alpine")
                    .WithDatabase("financial_accounting")
                    .WithUsername("test")
                    .WithPassword("test")
                    .Build()

                do! postgres.StartAsync()

                let connectionString = postgres.GetConnectionString()
                factory <- new WebApplicationFactory<FinancialAccounting.Api.IApiMarker>()
                factory <- factory.WithWebHostBuilder(fun builder ->
                    builder.UseSetting("ConnectionStrings:FinancialAccounting", connectionString) |> ignore
                )

                client <- factory.CreateClient()
            }

        member _.DisposeAsync() =
            task {
                if client <> null then
                    client.Dispose()
                if factory <> null then
                    do! factory.DisposeAsync().AsTask()
                if postgres <> null then
                    do! postgres.DisposeAsync().AsTask()
            }

    [<Fact>]
    member _.``勘定科目を作成できる``() =
        task {
            // Arrange - シードデータと重複しないコードを使用
            let uniqueCode = $"TEST{DateTime.UtcNow.Ticks % 10000L}"
            let request = {|
                AccountCode = uniqueCode
                AccountName = "Test Cash Account"
                AccountNameKana = "テストげんきん"
                AccountType = "Asset"
                IsSummaryAccount = false
                BsPlType = "B"
                TransactionElementType = "Debit"
                ExpenseType = ""
                DisplayOrder = 10
                IsAggregationTarget = true
                TaxCode = ""
            |}

            // Act
            let! response = client.PostAsJsonAsync("/api/accounts", request)

            // Assert
            Assert.Equal(HttpStatusCode.Created, response.StatusCode)

            let! account = response.Content.ReadFromJsonAsync<AccountResponseDto>()
            Assert.NotNull(account)
            Assert.True(account.AccountId > 0)
            Assert.Equal(uniqueCode, account.AccountCode)
            Assert.Equal("Test Cash Account", account.AccountName)
        }

    [<Fact>]
    member _.``IDで勘定科目を取得できる``() =
        task {
            // Arrange - シードデータと重複しないコードを使用
            let uniqueCode = $"AR{DateTime.UtcNow.Ticks % 10000L}"
            let createRequest = {|
                AccountCode = uniqueCode
                AccountName = "Test Accounts Receivable"
                AccountNameKana = ""
                AccountType = "Asset"
                IsSummaryAccount = false
                BsPlType = "B"
                TransactionElementType = "Debit"
                ExpenseType = ""
                DisplayOrder = 20
                IsAggregationTarget = true
                TaxCode = ""
            |}
            let! createResponse = client.PostAsJsonAsync("/api/accounts", createRequest)
            let! created = createResponse.Content.ReadFromJsonAsync<AccountResponseDto>()

            // Act
            let! response = client.GetAsync($"/api/accounts/{created.AccountId}")

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)
            let! account = response.Content.ReadFromJsonAsync<AccountResponseDto>()
            Assert.Equal(uniqueCode, account.AccountCode)
        }

    [<Fact>]
    member _.``勘定科目コードで勘定科目を取得できる``() =
        task {
            // Arrange - シードデータと重複しないコードを使用
            let uniqueCode = $"INV{DateTime.UtcNow.Ticks % 10000L}"
            let createRequest = {|
                AccountCode = uniqueCode
                AccountName = "Test Inventory"
                AccountNameKana = ""
                AccountType = "Asset"
                IsSummaryAccount = false
                BsPlType = "B"
                TransactionElementType = "Debit"
                ExpenseType = ""
                DisplayOrder = 30
                IsAggregationTarget = true
                TaxCode = ""
            |}
            let! _ = client.PostAsJsonAsync("/api/accounts", createRequest)

            // Act
            let! response = client.GetAsync($"/api/accounts/code/{uniqueCode}")

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)
            let! account = response.Content.ReadFromJsonAsync<AccountResponseDto>()
            Assert.Equal("Test Inventory", account.AccountName)
        }

    [<Fact>]
    member _.``全ての勘定科目を取得できる``() =
        task {
            // Act - シードデータがあるので作成不要
            let! response = client.GetAsync("/api/accounts")

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)
            let! accounts = response.Content.ReadFromJsonAsync<AccountResponseDto[]>()
            Assert.NotEmpty(accounts)
        }

    [<Fact>]
    member _.``勘定科目種別で勘定科目を取得できる``() =
        task {
            // Act - シードデータに「Revenue」科目があるのでそれを取得
            let! response = client.GetAsync("/api/accounts/type/Revenue")

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)
            let! accounts = response.Content.ReadFromJsonAsync<AccountResponseDto[]>()
            Assert.NotEmpty(accounts)
            Assert.All(accounts, fun a -> Assert.Equal("Revenue", a.AccountType))
        }

    [<Fact>]
    member _.``勘定科目を更新できる``() =
        task {
            // Arrange - シードデータと重複しないコードを使用
            let uniqueCode = $"UPD{DateTime.UtcNow.Ticks % 10000L}"
            let createRequest = {|
                AccountCode = uniqueCode
                AccountName = "Before Update"
                AccountNameKana = ""
                AccountType = "Asset"
                IsSummaryAccount = false
                BsPlType = "B"
                TransactionElementType = "Debit"
                ExpenseType = ""
                DisplayOrder = 0
                IsAggregationTarget = true
                TaxCode = ""
            |}
            let! createResponse = client.PostAsJsonAsync("/api/accounts", createRequest)
            let! created = createResponse.Content.ReadFromJsonAsync<AccountResponseDto>()

            let updateRequest = {|
                AccountName = "After Update"
                AccountNameKana = "アップデート"
                IsSummaryAccount = true
                ExpenseType = ""
                DisplayOrder = 100
                IsAggregationTarget = false
                TaxCode = "T01"
            |}

            // Act
            let! response = client.PutAsJsonAsync($"/api/accounts/{created.AccountId}", updateRequest)

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)
            let! account = response.Content.ReadFromJsonAsync<AccountResponseDto>()
            Assert.Equal("After Update", account.AccountName)
            Assert.Equal(100, account.DisplayOrder)
        }

    [<Fact>]
    member _.``勘定科目を削除できる``() =
        task {
            // Arrange - シードデータと重複しないコードを使用
            let uniqueCode = $"DEL{DateTime.UtcNow.Ticks % 10000L}"
            let createRequest = {|
                AccountCode = uniqueCode
                AccountName = "To Delete"
                AccountNameKana = ""
                AccountType = "Asset"
                IsSummaryAccount = false
                BsPlType = "B"
                TransactionElementType = "Debit"
                ExpenseType = ""
                DisplayOrder = 0
                IsAggregationTarget = true
                TaxCode = ""
            |}
            let! createResponse = client.PostAsJsonAsync("/api/accounts", createRequest)
            let! created = createResponse.Content.ReadFromJsonAsync<AccountResponseDto>()

            // Act
            let! response = client.DeleteAsync($"/api/accounts/{created.AccountId}")

            // Assert
            Assert.Equal(HttpStatusCode.NoContent, response.StatusCode)

            // 削除されたことを確認
            let! getResponse = client.GetAsync($"/api/accounts/{created.AccountId}")
            Assert.Equal(HttpStatusCode.NotFound, getResponse.StatusCode)
        }

    [<Fact>]
    member _.``存在しないIDの勘定科目は NotFound を返す``() =
        task {
            // Act
            let! response = client.GetAsync("/api/accounts/99999")

            // Assert
            Assert.Equal(HttpStatusCode.NotFound, response.StatusCode)
        }

    [<Fact>]
    member _.``無効な勘定科目種別は BadRequest を返す``() =
        task {
            // Act
            let! response = client.GetAsync("/api/accounts/type/InvalidType")

            // Assert
            Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        }
