using System.Net;
using System.Net.Http.Json;
using AccountingSystem.Infrastructure.Web.Dtos;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using Dapper;
using FluentAssertions;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Npgsql;
using Testcontainers.PostgreSql;
using Xunit;

namespace AccountingSystem.Tests.Integration;

/// <summary>
/// 財務諸表 API 統合テスト
/// </summary>
public class FinancialStatementApiTest : IAsyncLifetime
{
    private PostgreSqlContainer? _postgres;
    private WebApplicationFactory<Program>? _factory;
    private HttpClient? _client;

    static FinancialStatementApiTest()
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

        // 勘定科目マスタ
        await connection.ExecuteAsync(@"
            CREATE TABLE IF NOT EXISTS ""勘定科目マスタ"" (
                ""勘定科目コード"" VARCHAR(10) PRIMARY KEY,
                ""勘定科目名"" VARCHAR(100) NOT NULL,
                ""勘定科目略名"" VARCHAR(50),
                ""勘定科目カナ"" VARCHAR(100),
                ""BSPL区分"" CHAR(1) NOT NULL,
                ""貸借区分"" VARCHAR(10),
                ""取引要素区分"" VARCHAR(10),
                ""集計区分"" VARCHAR(10),
                ""費用区分"" VARCHAR(10),
                ""表示順"" INTEGER
            )
        ");

        // 日次勘定科目残高
        await connection.ExecuteAsync(@"
            CREATE TABLE IF NOT EXISTS ""日次勘定科目残高"" (
                ""起票日"" DATE NOT NULL,
                ""勘定科目コード"" VARCHAR(10) NOT NULL,
                ""補助科目コード"" VARCHAR(10) NOT NULL DEFAULT '',
                ""部門コード"" VARCHAR(10) NOT NULL DEFAULT '',
                ""プロジェクトコード"" VARCHAR(10) NOT NULL DEFAULT '',
                ""決算仕訳フラグ"" INTEGER NOT NULL DEFAULT 0,
                ""借方金額"" DECIMAL(15, 2) NOT NULL DEFAULT 0,
                ""貸方金額"" DECIMAL(15, 2) NOT NULL DEFAULT 0,
                PRIMARY KEY (""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"", ""プロジェクトコード"", ""決算仕訳フラグ"")
            )
        ");
    }

    private static async Task SetupTestDataAsync(string connectionString)
    {
        await using var connection = new NpgsqlConnection(connectionString);
        await connection.OpenAsync();

        // 貸借対照表用テストデータ
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""BSPL区分"", ""取引要素区分""
            ) VALUES
            ('1110', '普通預金', 'B', '1'),
            ('1410', '建物', 'B', '1'),
            ('2110', '買掛金', 'B', '2'),
            ('2510', '長期借入金', 'B', '2'),
            ('3110', '資本金', 'B', '3'),
            ('4110', '売上高', 'P', '4'),
            ('5110', '売上原価', 'P', '5'),
            ('6110', '給与手当', 'P', '5'),
            ('6210', '福利厚生費', 'P', '5')
            ON CONFLICT (""勘定科目コード"") DO NOTHING
        ");

        // 費用区分を更新
        await connection.ExecuteAsync(@"
            UPDATE ""勘定科目マスタ"" SET ""費用区分"" = '1' WHERE ""勘定科目コード"" = '5110';
            UPDATE ""勘定科目マスタ"" SET ""費用区分"" = '2' WHERE ""勘定科目コード"" IN ('6110', '6210');
        ");

        var asOfDate = new DateOnly(2024, 1, 31);
        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES
            (@AsOfDate, '1110', '', '', '', 0, 8000000, 0),
            (@AsOfDate, '1410', '', '', '', 0, 2000000, 0),
            (@AsOfDate, '2110', '', '', '', 0, 0, 500000),
            (@AsOfDate, '2510', '', '', '', 0, 0, 4500000),
            (@AsOfDate, '3110', '', '', '', 0, 0, 5000000),
            (@AsOfDate, '4110', '', '', '', 0, 0, 10000000),
            (@AsOfDate, '5110', '', '', '', 0, 6000000, 0),
            (@AsOfDate, '6110', '', '', '', 0, 1500000, 0),
            (@AsOfDate, '6210', '', '', '', 0, 500000, 0)
        ", new { AsOfDate = asOfDate });
    }

    [Fact(DisplayName = "GET /api/v1/financial-statements/balance-sheet - 貸借対照表を取得できる")]
    public async Task GetBalanceSheet_Returns200()
    {
        // Act
        var response = await _client!.GetAsync(
            "/api/v1/financial-statements/balance-sheet?asOfDate=2024-01-31");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var balanceSheet = await response.Content.ReadFromJsonAsync<BalanceSheetResponse>();
        balanceSheet.Should().NotBeNull();
        balanceSheet!.AsOfDate.Should().Be(new DateOnly(2024, 1, 31));
        balanceSheet.Assets.Should().NotBeEmpty();
        balanceSheet.Liabilities.Should().NotBeEmpty();
        balanceSheet.Equity.Should().NotBeEmpty();
        balanceSheet.TotalAssets.Should().Be(10000000m);
        balanceSheet.TotalLiabilitiesAndEquity.Should().Be(10000000m);
    }

    [Fact(DisplayName = "GET /api/v1/financial-statements/income-statement - 損益計算書を取得できる")]
    public async Task GetIncomeStatement_Returns200()
    {
        // Act
        var response = await _client!.GetAsync(
            "/api/v1/financial-statements/income-statement?fromDate=2024-01-01&toDate=2024-01-31");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var incomeStatement = await response.Content.ReadFromJsonAsync<IncomeStatementResponse>();
        incomeStatement.Should().NotBeNull();
        incomeStatement!.FromDate.Should().Be(new DateOnly(2024, 1, 1));
        incomeStatement.ToDate.Should().Be(new DateOnly(2024, 1, 31));
        incomeStatement.Revenues.Should().NotBeEmpty();
        incomeStatement.Expenses.Should().NotBeEmpty();
        incomeStatement.TotalRevenues.Should().Be(10000000m);
        incomeStatement.NetIncome.Should().Be(2000000m);
    }

    [Fact(DisplayName = "GET /api/v1/financial-statements/financial-ratios - 財務指標を取得できる")]
    public async Task GetFinancialRatios_Returns200()
    {
        // Act
        var response = await _client!.GetAsync(
            "/api/v1/financial-statements/financial-ratios?asOfDate=2024-01-31&fromDate=2024-01-01&toDate=2024-01-31");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var ratios = await response.Content.ReadFromJsonAsync<FinancialRatiosResponse>();
        ratios.Should().NotBeNull();
        ratios!.CurrentRatio.Should().BeGreaterThan(0);
        ratios.EquityRatio.Should().Be(50.00m);
        ratios.GrossProfitMargin.Should().Be(40.00m);
        ratios.OperatingProfitMargin.Should().Be(20.00m);
        ratios.NetProfitMargin.Should().Be(20.00m);
        ratios.Roa.Should().Be(20.00m);
        ratios.Roe.Should().Be(40.00m);
    }
}
