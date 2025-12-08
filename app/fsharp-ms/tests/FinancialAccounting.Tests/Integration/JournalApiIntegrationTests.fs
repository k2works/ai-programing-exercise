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
/// 仕訳レスポンス DTO
/// </summary>
[<CLIMutable>]
type JournalResponseDto = {
    JournalId: int
    JournalDate: DateTime
    Description: string
    FiscalYear: int
    Entries: JournalEntryResponseDto[]
}
and [<CLIMutable>] JournalEntryResponseDto = {
    AccountCode: string
    DebitAmount: decimal
    CreditAmount: decimal
    Description: string
}

/// <summary>
/// エラーレスポンス DTO
/// </summary>
[<CLIMutable>]
type ErrorResponseDto = {
    error: string
}

/// <summary>
/// 財務会計 API 統合テスト（Testcontainers 使用）
/// </summary>
type JournalApiIntegrationTests() =
    let mutable postgres: PostgreSqlContainer = null
    let mutable factory: WebApplicationFactory<FinancialAccounting.Api.IApiMarker> = null
    let mutable client: HttpClient = null

    interface IAsyncLifetime with
        member _.InitializeAsync() =
            task {
                // Windows Docker Desktop 用に環境変数を設定
                DockerHostConfig.configure ()

                // PostgreSQL コンテナを起動
                postgres <- PostgreSqlBuilder()
                    .WithImage("postgres:16-alpine")
                    .WithDatabase("financial_accounting")
                    .WithUsername("test")
                    .WithPassword("test")
                    .Build()

                do! postgres.StartAsync()

                // WebApplicationFactory で API サーバーを起動
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
    member _.``仕訳を作成できる``() =
        task {
            // Arrange
            let request = {|
                JournalDate = DateTime.UtcNow.Date
                Description = "Test Journal"
                FiscalYear = 2024
                Entries = [|
                    {| AccountCode = "1120"; DebitAmount = 100000m; CreditAmount = 0m; Description = "Accounts Receivable" |}
                    {| AccountCode = "4110"; DebitAmount = 0m; CreditAmount = 100000m; Description = "Sales Revenue" |}
                |]
            |}

            // Act
            let! response = client.PostAsJsonAsync("/api/journals", request)

            // Assert
            let! content = response.Content.ReadAsStringAsync()
            Assert.Equal(HttpStatusCode.Created, response.StatusCode)

            let! journal = response.Content.ReadFromJsonAsync<JournalResponseDto>()
            Assert.NotNull(journal)
            Assert.True(journal.JournalId > 0)
            Assert.Equal(2, journal.Entries.Length)
        }

    [<Fact>]
    member _.``作成した仕訳を ID で取得できる``() =
        task {
            // Arrange - まず仕訳を作成
            let createRequest = {|
                JournalDate = DateTime.UtcNow.Date
                Description = "Test Journal for Get"
                FiscalYear = 2024
                Entries = [|
                    {| AccountCode = "1110"; DebitAmount = 50000m; CreditAmount = 0m; Description = "Cash" |}
                    {| AccountCode = "4110"; DebitAmount = 0m; CreditAmount = 50000m; Description = "Sales" |}
                |]
            |}
            let! createResponse = client.PostAsJsonAsync("/api/journals", createRequest)
            let! createdJournal = createResponse.Content.ReadFromJsonAsync<JournalResponseDto>()

            // Act
            let! response = client.GetAsync($"/api/journals/{createdJournal.JournalId}")

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)

            let! journal = response.Content.ReadFromJsonAsync<JournalResponseDto>()
            Assert.NotNull(journal)
            Assert.Equal(createdJournal.JournalId, journal.JournalId)
            Assert.Equal("Test Journal for Get", journal.Description)
        }

    [<Fact>]
    member _.``会計年度で仕訳一覧を取得できる``() =
        task {
            // Arrange - まず仕訳を作成
            let createRequest = {|
                JournalDate = DateTime.UtcNow.Date
                Description = "Test Journal for Fiscal Year"
                FiscalYear = 2025
                Entries = [|
                    {| AccountCode = "1110"; DebitAmount = 30000m; CreditAmount = 0m; Description = "Cash" |}
                    {| AccountCode = "4110"; DebitAmount = 0m; CreditAmount = 30000m; Description = "Sales" |}
                |]
            |}
            let! _ = client.PostAsJsonAsync("/api/journals", createRequest)

            // Act
            let! response = client.GetAsync("/api/journals?fiscalYear=2025")

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode)

            let! journals = response.Content.ReadFromJsonAsync<JournalResponseDto[]>()
            Assert.NotNull(journals)
            Assert.True(journals.Length > 0)
            Assert.All(journals, fun j -> Assert.Equal(2025, j.FiscalYear))
        }

    [<Fact>]
    member _.``借方と貸方が一致しない仕訳は作成できない``() =
        task {
            // Arrange - 借方と貸方が一致しない仕訳
            let request = {|
                JournalDate = DateTime.UtcNow.Date
                Description = "Unbalanced Journal"
                FiscalYear = 2024
                Entries = [|
                    {| AccountCode = "1110"; DebitAmount = 100000m; CreditAmount = 0m; Description = "Cash" |}
                    {| AccountCode = "4110"; DebitAmount = 0m; CreditAmount = 50000m; Description = "Sales (unbalanced)" |}
                |]
            |}

            // Act
            let! response = client.PostAsJsonAsync("/api/journals", request)

            // Assert - バリデーションエラーで BadRequest が返される
            Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        }

    [<Fact>]
    member _.``存在しない ID の仕訳は NotFound を返す``() =
        task {
            // Act
            let! response = client.GetAsync("/api/journals/99999")

            // Assert
            Assert.Equal(HttpStatusCode.NotFound, response.StatusCode)
        }
