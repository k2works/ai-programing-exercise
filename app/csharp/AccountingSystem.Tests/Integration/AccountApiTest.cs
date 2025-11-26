using System.Net;
using System.Net.Http.Json;
using AccountingSystem.Api.Dtos;
using AccountingSystem.Infrastructure.Repositories;
using Dapper;
using FluentAssertions;
using Microsoft.AspNetCore.Mvc.Testing;
using Npgsql;
using Testcontainers.PostgreSql;
using Xunit;

namespace AccountingSystem.Tests.Integration;

/// <summary>
/// 勘定科目 API 統合テスト
/// </summary>
public class AccountApiTest : IAsyncLifetime
{
    private PostgreSqlContainer? _postgres;
    private WebApplicationFactory<Program>? _factory;
    private HttpClient? _client;

    static AccountApiTest()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public async Task InitializeAsync()
    {
        // TestContainers で PostgreSQL を起動
        _postgres = new PostgreSqlBuilder()
            .WithImage("postgres:16-alpine")
            .WithDatabase("testdb")
            .WithUsername("test")
            .WithPassword("test")
            .Build();

        await _postgres.StartAsync();

        // マイグレーション実行
        var connectionString = _postgres.GetConnectionString();
        await RunMigrationAsync(connectionString);
        await SetupTestDataAsync(connectionString);

        // WebApplicationFactory で API サーバーを起動
        _factory = new WebApplicationFactory<Program>()
            .WithWebHostBuilder(builder =>
            {
                builder.UseSetting(
                    "ConnectionStrings:DefaultConnection",
                    connectionString);
            });

        _client = _factory.CreateClient();
    }

    public async Task DisposeAsync()
    {
        _client?.Dispose();
        _factory?.Dispose();

        if (_postgres != null)
        {
            await _postgres.DisposeAsync();
        }
    }

    private static async Task RunMigrationAsync(string connectionString)
    {
        await using var connection = new NpgsqlConnection(connectionString);
        await connection.OpenAsync();

        // account_type ENUM型を作成
        await connection.ExecuteAsync(@"
            DO $$ BEGIN
                CREATE TYPE account_type AS ENUM (
                    '資産', '負債', '純資産', '収益', '費用'
                );
            EXCEPTION
                WHEN duplicate_object THEN null;
            END $$;
        ");

        // 勘定科目マスタ
        await connection.ExecuteAsync(@"
            CREATE TABLE IF NOT EXISTS ""勘定科目マスタ"" (
                ""勘定科目ID"" SERIAL PRIMARY KEY,
                ""勘定科目コード"" VARCHAR(10) UNIQUE NOT NULL,
                ""勘定科目名"" VARCHAR(100) NOT NULL,
                ""勘定科目カナ"" VARCHAR(100),
                ""勘定科目種別"" account_type NOT NULL,
                ""合計科目"" BOOLEAN NOT NULL DEFAULT FALSE,
                ""BSPL区分"" CHAR(1),
                ""取引要素区分"" VARCHAR(10),
                ""費用区分"" VARCHAR(10),
                ""表示順序"" INTEGER NOT NULL DEFAULT 0,
                ""集計対象"" BOOLEAN NOT NULL DEFAULT TRUE,
                ""課税取引コード"" VARCHAR(10),
                ""残高"" DECIMAL(15, 2) NOT NULL DEFAULT 0,
                ""作成日時"" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                ""更新日時"" TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ");
    }

    private static async Task SetupTestDataAsync(string connectionString)
    {
        await using var connection = new NpgsqlConnection(connectionString);
        await connection.OpenAsync();

        // テストデータ投入
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""合計科目"", ""BSPL区分"", ""表示順序""
            ) VALUES
            ('1110', '普通預金', '資産', false, 'B', 10),
            ('1120', '当座預金', '資産', false, 'B', 20),
            ('2110', '買掛金', '負債', false, 'B', 30),
            ('4110', '売上高', '収益', false, 'P', 40)
            ON CONFLICT (""勘定科目コード"") DO NOTHING
        ");
    }

    [Fact(DisplayName = "GET /api/v1/accounts - すべての勘定科目を取得できる")]
    public async Task GetAllAccounts_Returns200()
    {
        // Act
        var response = await _client!.GetAsync("/api/v1/accounts");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var accounts = await response.Content.ReadFromJsonAsync<List<AccountResponse>>();
        accounts.Should().NotBeNull();
        accounts!.Count.Should().BeGreaterThanOrEqualTo(4);
    }

    [Fact(DisplayName = "GET /api/v1/accounts/{accountCode} - 科目コードで勘定科目を取得できる")]
    public async Task GetAccountByCode_Returns200()
    {
        // Act
        var response = await _client!.GetAsync("/api/v1/accounts/1110");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var account = await response.Content.ReadFromJsonAsync<AccountResponse>();
        account.Should().NotBeNull();
        account!.AccountCode.Should().Be("1110");
        account.AccountName.Should().Be("普通預金");
    }

    [Fact(DisplayName = "GET /api/v1/accounts/{accountCode} - 存在しない科目コードで404を返す")]
    public async Task GetAccountByCode_NotFound_Returns404()
    {
        // Act
        var response = await _client!.GetAsync("/api/v1/accounts/9999");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact(DisplayName = "GET /api/v1/accounts/by-bspl-type/{bsplType} - BSPL区分で勘定科目を取得できる")]
    public async Task GetAccountsByBsplType_Returns200()
    {
        // Act
        var response = await _client!.GetAsync("/api/v1/accounts/by-bspl-type/B");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var accounts = await response.Content.ReadFromJsonAsync<List<AccountResponse>>();
        accounts.Should().NotBeNull();
        accounts!.Should().OnlyContain(a => a.BsplType == "B");
    }

    [Fact(DisplayName = "GET /api/v1/accounts/by-bspl-type/{bsplType} - 無効なBSPL区分で400を返す")]
    public async Task GetAccountsByBsplType_InvalidType_Returns400()
    {
        // Act
        var response = await _client!.GetAsync("/api/v1/accounts/by-bspl-type/X");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.BadRequest);
    }

    [Fact(DisplayName = "GET /api/v1/accounts/by-type/{accountType} - 勘定科目種別で勘定科目を取得できる")]
    public async Task GetAccountsByType_Returns200()
    {
        // Act
        var response = await _client!.GetAsync("/api/v1/accounts/by-type/資産");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var accounts = await response.Content.ReadFromJsonAsync<List<AccountResponse>>();
        accounts.Should().NotBeNull();
        accounts!.Should().OnlyContain(a => a.AccountType == "資産");
    }

    [Fact(DisplayName = "POST /api/v1/accounts - 新しい勘定科目を作成できる")]
    public async Task CreateAccount_Returns201()
    {
        // Arrange
        var request = new AccountRequest
        {
            AccountCode = "1130",
            AccountName = "定期預金",
            AccountType = "資産",
            IsSummaryAccount = false,
            BsplType = "B",
            DisplayOrder = 25,
            IsAggregationTarget = true
        };

        // Act
        var response = await _client!.PostAsJsonAsync("/api/v1/accounts", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        response.Headers.Location.Should().NotBeNull();

        var created = await response.Content.ReadFromJsonAsync<AccountResponse>();
        created.Should().NotBeNull();
        created!.AccountCode.Should().Be("1130");
        created.AccountName.Should().Be("定期預金");
    }

    [Fact(DisplayName = "POST /api/v1/accounts - 重複する科目コードで409を返す")]
    public async Task CreateAccount_Duplicate_Returns409()
    {
        // Arrange
        var request = new AccountRequest
        {
            AccountCode = "1110", // 既存のコード
            AccountName = "重複テスト",
            AccountType = "資産",
            IsSummaryAccount = false
        };

        // Act
        var response = await _client!.PostAsJsonAsync("/api/v1/accounts", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Conflict);
    }

    [Fact(DisplayName = "PUT /api/v1/accounts/{accountCode} - 勘定科目を更新できる")]
    public async Task UpdateAccount_Returns200()
    {
        // Arrange
        var request = new AccountRequest
        {
            AccountCode = "1120",
            AccountName = "当座預金（更新）",
            AccountType = "資産",
            IsSummaryAccount = false,
            BsplType = "B",
            DisplayOrder = 20
        };

        // Act
        var response = await _client!.PutAsJsonAsync("/api/v1/accounts/1120", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var updated = await response.Content.ReadFromJsonAsync<AccountResponse>();
        updated.Should().NotBeNull();
        updated!.AccountName.Should().Be("当座預金（更新）");
    }

    [Fact(DisplayName = "PUT /api/v1/accounts/{accountCode} - 存在しない科目コードで404を返す")]
    public async Task UpdateAccount_NotFound_Returns404()
    {
        // Arrange
        var request = new AccountRequest
        {
            AccountCode = "9999",
            AccountName = "存在しない",
            AccountType = "資産",
            IsSummaryAccount = false
        };

        // Act
        var response = await _client!.PutAsJsonAsync("/api/v1/accounts/9999", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact(DisplayName = "DELETE /api/v1/accounts/{accountCode} - 勘定科目を削除できる")]
    public async Task DeleteAccount_Returns204()
    {
        // Act
        var response = await _client!.DeleteAsync("/api/v1/accounts/4110");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NoContent);

        // 削除確認
        var getResponse = await _client!.GetAsync("/api/v1/accounts/4110");
        getResponse.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact(DisplayName = "DELETE /api/v1/accounts/{accountCode} - 存在しない科目コードで404を返す")]
    public async Task DeleteAccount_NotFound_Returns404()
    {
        // Act
        var response = await _client!.DeleteAsync("/api/v1/accounts/9999");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }
}
