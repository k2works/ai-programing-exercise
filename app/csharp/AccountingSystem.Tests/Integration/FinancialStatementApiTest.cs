using System.Net;
using System.Net.Http.Json;
using AccountingSystem.Infrastructure.Web.Dtos;
using FluentAssertions;
using Xunit;

namespace AccountingSystem.Tests.Integration;

/// <summary>
/// 財務諸表 API 統合テスト
/// </summary>
public class FinancialStatementApiTest : ApiTestBase
{
    protected override async Task OnInitializedAsync()
    {
        // 勘定科目マスタにデータを追加（残高カラムなどの不足対応）
        await ExecuteSqlAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""合計科目"", ""BSPL区分"", ""取引要素区分"", ""費用区分""
            ) VALUES
            ('1110', '普通預金', '資産', false, 'B', '1', NULL),
            ('1410', '建物', '資産', false, 'B', '1', NULL),
            ('2110', '買掛金', '負債', false, 'B', '2', NULL),
            ('2510', '長期借入金', '負債', false, 'B', '2', NULL),
            ('3110', '資本金', '純資産', false, 'B', '3', NULL),
            ('4110', '売上高', '収益', false, 'P', '4', NULL),
            ('5110', '売上原価', '費用', false, 'P', '5', '1'),
            ('6110', '給与手当', '費用', false, 'P', '5', '2'),
            ('6210', '福利厚生費', '費用', false, 'P', '5', '2')
            ON CONFLICT (""勘定科目コード"") DO NOTHING
        ");

        var asOfDate = new DateOnly(2024, 1, 31);
        await ExecuteSqlAsync(@"
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
        var response = await Client.GetAsync(
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
        var response = await Client.GetAsync(
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
        var response = await Client.GetAsync(
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
