using System.Net;
using System.Net.Http.Json;
using AccountingSystem.Infrastructure.Web.Dtos;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using Dapper;
using FluentAssertions;
using Microsoft.AspNetCore.Mvc.Testing;
using Npgsql;
using Testcontainers.PostgreSql;
using Xunit;

namespace AccountingSystem.Tests.Integration;

/// <summary>
/// 仕訳 API 統合テスト
/// </summary>
public class JournalApiTest : IAsyncLifetime
{
    private PostgreSqlContainer? _postgres;
    private WebApplicationFactory<Program>? _factory;
    private HttpClient? _client;

    static JournalApiTest()
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

        // 仕訳テーブル
        await connection.ExecuteAsync(@"
            CREATE TABLE IF NOT EXISTS ""仕訳"" (
                ""仕訳伝票番号"" VARCHAR(20) PRIMARY KEY,
                ""起票日"" DATE NOT NULL,
                ""入力日"" DATE NOT NULL,
                ""決算仕訳フラグ"" INTEGER NOT NULL DEFAULT 0,
                ""単振フラグ"" INTEGER NOT NULL DEFAULT 0,
                ""仕訳伝票区分"" INTEGER NOT NULL DEFAULT 0,
                ""定期計上フラグ"" INTEGER NOT NULL DEFAULT 0,
                ""社員コード"" VARCHAR(10),
                ""部門コード"" VARCHAR(10),
                ""赤伝フラグ"" INTEGER NOT NULL DEFAULT 0,
                ""赤黒伝票番号"" VARCHAR(20),
                ""作成日時"" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                ""更新日時"" TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ");

        // 仕訳明細テーブル
        await connection.ExecuteAsync(@"
            CREATE TABLE IF NOT EXISTS ""仕訳明細"" (
                ""仕訳伝票番号"" VARCHAR(20) NOT NULL,
                ""仕訳行番号"" INTEGER NOT NULL,
                ""行摘要"" VARCHAR(200) NOT NULL,
                ""作成日時"" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                ""更新日時"" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                PRIMARY KEY (""仕訳伝票番号"", ""仕訳行番号""),
                FOREIGN KEY (""仕訳伝票番号"") REFERENCES ""仕訳""(""仕訳伝票番号"") ON DELETE CASCADE
            )
        ");

        // 仕訳貸借明細テーブル
        await connection.ExecuteAsync(@"
            CREATE TABLE IF NOT EXISTS ""仕訳貸借明細"" (
                ""仕訳伝票番号"" VARCHAR(20) NOT NULL,
                ""仕訳行番号"" INTEGER NOT NULL,
                ""仕訳行貸借区分"" CHAR(1) NOT NULL,
                ""通貨コード"" VARCHAR(3) NOT NULL DEFAULT 'JPY',
                ""為替レート"" DECIMAL(10, 4) NOT NULL DEFAULT 1,
                ""部門コード"" VARCHAR(10),
                ""プロジェクトコード"" VARCHAR(10),
                ""勘定科目コード"" VARCHAR(10) NOT NULL,
                ""補助科目コード"" VARCHAR(10),
                ""仕訳金額"" DECIMAL(15, 2) NOT NULL,
                ""基軸換算仕訳金額"" DECIMAL(15, 2) NOT NULL,
                ""消費税区分"" VARCHAR(2),
                ""消費税率"" INTEGER,
                ""消費税計算区分"" VARCHAR(1),
                ""期日"" DATE,
                ""資金繰フラグ"" INTEGER NOT NULL DEFAULT 0,
                ""セグメントコード"" VARCHAR(10),
                ""相手勘定科目コード"" VARCHAR(10),
                ""相手補助科目コード"" VARCHAR(10),
                ""付箋コード"" VARCHAR(10),
                ""付箋内容"" VARCHAR(200),
                ""作成日時"" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                ""更新日時"" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                PRIMARY KEY (""仕訳伝票番号"", ""仕訳行番号"", ""仕訳行貸借区分""),
                FOREIGN KEY (""仕訳伝票番号"", ""仕訳行番号"")
                    REFERENCES ""仕訳明細""(""仕訳伝票番号"", ""仕訳行番号"") ON DELETE CASCADE
            )
        ");
    }

    private static JournalRequest CreateValidJournalRequest(string journalNo)
    {
        return new JournalRequest
        {
            JournalNo = journalNo,
            JournalDate = new DateOnly(2024, 1, 15),
            InputDate = new DateOnly(2024, 1, 15),
            SettlementFlag = 0,
            SingleEntryFlag = 0,
            JournalType = 1,
            RecurringFlag = 0,
            RedSlipFlag = 0,
            Details = new List<JournalDetailRequest>
            {
                new()
                {
                    LineNumber = 1,
                    Description = "売上取引",
                    Items = new List<JournalDetailItemRequest>
                    {
                        new()
                        {
                            DebitCreditFlag = "D",
                            AccountCode = "1110",
                            Amount = 100000m,
                            CurrencyCode = "JPY",
                            ExchangeRate = 1m,
                            CashFlowFlag = 0
                        },
                        new()
                        {
                            DebitCreditFlag = "C",
                            AccountCode = "4110",
                            Amount = 100000m,
                            CurrencyCode = "JPY",
                            ExchangeRate = 1m,
                            CashFlowFlag = 0
                        }
                    }
                }
            }
        };
    }

    [Fact(DisplayName = "POST /api/v1/journals - 仕訳を作成できる")]
    public async Task CreateJournal_Returns201()
    {
        // Arrange
        var request = CreateValidJournalRequest("J20240115001");

        // Act
        var response = await _client!.PostAsJsonAsync("/api/v1/journals", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        response.Headers.Location.Should().NotBeNull();

        var created = await response.Content.ReadFromJsonAsync<JournalResponse>();
        created.Should().NotBeNull();
        created!.JournalNo.Should().Be("J20240115001");
        created.Details.Should().HaveCount(1);
        created.Details[0].Items.Should().HaveCount(2);
    }

    [Fact(DisplayName = "POST /api/v1/journals - 貸借不一致で400を返す")]
    public async Task CreateJournal_UnbalancedEntry_Returns400()
    {
        // Arrange
        var request = new JournalRequest
        {
            JournalNo = "J20240115002",
            JournalDate = new DateOnly(2024, 1, 15),
            InputDate = new DateOnly(2024, 1, 15),
            SettlementFlag = 0,
            SingleEntryFlag = 0,
            JournalType = 1,
            RecurringFlag = 0,
            RedSlipFlag = 0,
            Details = new List<JournalDetailRequest>
            {
                new()
                {
                    LineNumber = 1,
                    Description = "不正な仕訳",
                    Items = new List<JournalDetailItemRequest>
                    {
                        new()
                        {
                            DebitCreditFlag = "D",
                            AccountCode = "1110",
                            Amount = 100000m,
                            CurrencyCode = "JPY",
                            ExchangeRate = 1m,
                            CashFlowFlag = 0
                        },
                        new()
                        {
                            DebitCreditFlag = "C",
                            AccountCode = "4110",
                            Amount = 50000m, // 貸借不一致
                            CurrencyCode = "JPY",
                            ExchangeRate = 1m,
                            CashFlowFlag = 0
                        }
                    }
                }
            }
        };

        // Act
        var response = await _client!.PostAsJsonAsync("/api/v1/journals", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.BadRequest);
    }

    [Fact(DisplayName = "POST /api/v1/journals - 重複する伝票番号で409を返す")]
    public async Task CreateJournal_Duplicate_Returns409()
    {
        // Arrange - 最初の仕訳を作成
        var request = CreateValidJournalRequest("J20240115003");
        await _client!.PostAsJsonAsync("/api/v1/journals", request);

        // Act - 同じ伝票番号で再度作成
        var response = await _client!.PostAsJsonAsync("/api/v1/journals", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Conflict);
    }

    [Fact(DisplayName = "GET /api/v1/journals/{journalNo} - 仕訳を取得できる")]
    public async Task GetJournal_Returns200()
    {
        // Arrange
        var request = CreateValidJournalRequest("J20240115004");
        await _client!.PostAsJsonAsync("/api/v1/journals", request);

        // Act
        var response = await _client!.GetAsync("/api/v1/journals/J20240115004");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var journal = await response.Content.ReadFromJsonAsync<JournalResponse>();
        journal.Should().NotBeNull();
        journal!.JournalNo.Should().Be("J20240115004");
        journal.JournalDate.Should().Be(new DateOnly(2024, 1, 15));
    }

    [Fact(DisplayName = "GET /api/v1/journals/{journalNo} - 存在しない伝票番号で404を返す")]
    public async Task GetJournal_NotFound_Returns404()
    {
        // Act
        var response = await _client!.GetAsync("/api/v1/journals/NOTEXIST");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact(DisplayName = "GET /api/v1/journals/{journalNo}/balance - 残高検証ができる")]
    public async Task ValidateBalance_Returns200()
    {
        // Arrange
        var request = CreateValidJournalRequest("J20240115005");
        await _client!.PostAsJsonAsync("/api/v1/journals", request);

        // Act
        var response = await _client!.GetAsync("/api/v1/journals/J20240115005/balance");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var balance = await response.Content.ReadFromJsonAsync<JournalBalanceResponse>();
        balance.Should().NotBeNull();
        balance!.JournalNo.Should().Be("J20240115005");
        balance.DebitTotal.Should().Be(100000m);
        balance.CreditTotal.Should().Be(100000m);
        balance.IsBalanced.Should().BeTrue();
    }

    [Fact(DisplayName = "DELETE /api/v1/journals/{journalNo} - 仕訳を削除できる")]
    public async Task DeleteJournal_Returns204()
    {
        // Arrange
        var request = CreateValidJournalRequest("J20240115006");
        await _client!.PostAsJsonAsync("/api/v1/journals", request);

        // Act
        var response = await _client!.DeleteAsync("/api/v1/journals/J20240115006");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NoContent);

        // 削除確認
        var getResponse = await _client!.GetAsync("/api/v1/journals/J20240115006");
        getResponse.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact(DisplayName = "DELETE /api/v1/journals/{journalNo} - 存在しない伝票番号で404を返す")]
    public async Task DeleteJournal_NotFound_Returns404()
    {
        // Act
        var response = await _client!.DeleteAsync("/api/v1/journals/NOTEXIST");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }
}
