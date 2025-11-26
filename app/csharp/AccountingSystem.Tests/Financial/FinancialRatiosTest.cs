using System.Data;
using AccountingSystem.Application.Services;
using AccountingSystem.Infrastructure.Repositories;
using Dapper;
using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Financial;

/// <summary>
/// 財務指標計算のテスト
/// </summary>
public class FinancialRatiosTest : DatabaseTestBase
{
    static FinancialRatiosTest()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    private async Task SetUpTestDataAsync(NpgsqlConnection connection)
    {
        // テストデータ：勘定科目マスタ
        // 取引要素区分: 1=資産, 2=負債, 3=純資産, 4=収益, 5=費用
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分"", ""費用区分""
            ) VALUES
            -- 貸借対照表科目
            ('1110', '現金', '資産', 'B', '1', NULL),
            ('1120', '普通預金', '資産', 'B', '1', NULL),
            ('1210', '売掛金', '資産', 'B', '1', NULL),
            ('1410', '建物', '資産', 'B', '1', NULL),
            ('2110', '買掛金', '負債', 'B', '2', NULL),
            ('2510', '長期借入金', '負債', 'B', '2', NULL),
            ('3110', '資本金', '純資産', 'B', '3', NULL),
            -- 損益計算書科目
            ('4110', '売上高', '収益', 'P', '4', NULL),
            ('5110', '売上原価', '費用', 'P', '5', '1'),
            ('6110', '販管費', '費用', 'P', '5', '2')
            ON CONFLICT (""勘定科目コード"") DO NOTHING
        ");

        // テストデータ：日次残高
        var asOfDate = new DateOnly(2024, 1, 31);
        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES
            -- 流動資産: 現金1,000,000 + 普通預金2,000,000 + 売掛金1,500,000 = 4,500,000
            (@AsOfDate, '1110', '', '', '', 0, 1000000, 0),
            (@AsOfDate, '1120', '', '', '', 0, 2000000, 0),
            (@AsOfDate, '1210', '', '', '', 0, 1500000, 0),
            -- 固定資産: 建物5,500,000
            (@AsOfDate, '1410', '', '', '', 0, 5500000, 0),
            -- 流動負債: 買掛金2,000,000
            (@AsOfDate, '2110', '', '', '', 0, 0, 2000000),
            -- 固定負債: 長期借入金3,000,000
            (@AsOfDate, '2510', '', '', '', 0, 0, 3000000),
            -- 純資産: 資本金5,000,000
            (@AsOfDate, '3110', '', '', '', 0, 0, 5000000),
            -- 売上高: 10,000,000
            (@AsOfDate, '4110', '', '', '', 0, 0, 10000000),
            -- 売上原価: 6,000,000
            (@AsOfDate, '5110', '', '', '', 0, 6000000, 0),
            -- 販管費: 2,000,000
            (@AsOfDate, '6110', '', '', '', 0, 2000000, 0)
        ", new { AsOfDate = asOfDate });
    }

    [Fact(DisplayName = "財務指標を計算できる")]
    public async Task Test_財務指標を計算できる()
    {
        // Given: テストデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);
        var fromDate = new DateOnly(2024, 1, 1);

        // 貸借対照表と損益計算書を生成
        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, asOfDate);

        // When: 財務指標を計算
        var ratios = service.CalculateFinancialRatios(balanceSheet, incomeStatement);

        // Then: 財務指標が計算されている
        ratios.Should().NotBeNull();
        ratios.CurrentRatio.Should().BeGreaterThan(0m);
        ratios.EquityRatio.Should().BeGreaterThan(0m);
        ratios.GrossProfitMargin.Should().BeGreaterThan(0m);
        ratios.OperatingProfitMargin.Should().BeGreaterThan(0m);
        ratios.NetProfitMargin.Should().BeGreaterThan(0m);
        ratios.Roa.Should().BeGreaterThan(0m);
        ratios.Roe.Should().BeGreaterThan(0m);
    }

    [Fact(DisplayName = "流動比率が正しく計算されている")]
    public async Task Test_流動比率が正しく計算されている()
    {
        // Given: テストデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);
        var fromDate = new DateOnly(2024, 1, 1);

        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, asOfDate);

        // When: 財務指標を計算
        var ratios = service.CalculateFinancialRatios(balanceSheet, incomeStatement);

        // Then: 流動比率 = 流動資産 / 流動負債 × 100
        // 流動資産 = 1,000,000 + 2,000,000 + 1,500,000 = 4,500,000
        // 流動負債 = 2,000,000
        // 流動比率 = 4,500,000 / 2,000,000 × 100 = 225%
        ratios.CurrentRatio.Should().Be(225.00m);
    }

    [Fact(DisplayName = "自己資本比率が正しく計算されている")]
    public async Task Test_自己資本比率が正しく計算されている()
    {
        // Given: テストデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);
        var fromDate = new DateOnly(2024, 1, 1);

        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, asOfDate);

        // When: 財務指標を計算
        var ratios = service.CalculateFinancialRatios(balanceSheet, incomeStatement);

        // Then: 自己資本比率 = 純資産 / 総資産 × 100
        // 純資産 = 5,000,000
        // 総資産 = 10,000,000
        // 自己資本比率 = 5,000,000 / 10,000,000 × 100 = 50%
        ratios.EquityRatio.Should().Be(50.00m);
    }

    [Fact(DisplayName = "売上総利益率が正しく計算されている")]
    public async Task Test_売上総利益率が正しく計算されている()
    {
        // Given: テストデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);
        var fromDate = new DateOnly(2024, 1, 1);

        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, asOfDate);

        // When: 財務指標を計算
        var ratios = service.CalculateFinancialRatios(balanceSheet, incomeStatement);

        // Then: 売上総利益率 = 売上総利益 / 売上高 × 100
        // 売上総利益 = 10,000,000 - 6,000,000 = 4,000,000
        // 売上総利益率 = 4,000,000 / 10,000,000 × 100 = 40%
        ratios.GrossProfitMargin.Should().Be(40.00m);
    }

    [Fact(DisplayName = "営業利益率が正しく計算されている")]
    public async Task Test_営業利益率が正しく計算されている()
    {
        // Given: テストデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);
        var fromDate = new DateOnly(2024, 1, 1);

        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, asOfDate);

        // When: 財務指標を計算
        var ratios = service.CalculateFinancialRatios(balanceSheet, incomeStatement);

        // Then: 営業利益率 = 営業利益 / 売上高 × 100
        // 営業利益 = 4,000,000 - 2,000,000 = 2,000,000
        // 営業利益率 = 2,000,000 / 10,000,000 × 100 = 20%
        ratios.OperatingProfitMargin.Should().Be(20.00m);
    }

    [Fact(DisplayName = "ROAが正しく計算されている")]
    public async Task Test_ROAが正しく計算されている()
    {
        // Given: テストデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);
        var fromDate = new DateOnly(2024, 1, 1);

        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, asOfDate);

        // When: 財務指標を計算
        var ratios = service.CalculateFinancialRatios(balanceSheet, incomeStatement);

        // Then: ROA = 当期純利益 / 総資産 × 100
        // 当期純利益 = 2,000,000
        // 総資産 = 10,000,000
        // ROA = 2,000,000 / 10,000,000 × 100 = 20%
        ratios.Roa.Should().Be(20.00m);
    }

    [Fact(DisplayName = "ROEが正しく計算されている")]
    public async Task Test_ROEが正しく計算されている()
    {
        // Given: テストデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);
        var fromDate = new DateOnly(2024, 1, 1);

        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, asOfDate);

        // When: 財務指標を計算
        var ratios = service.CalculateFinancialRatios(balanceSheet, incomeStatement);

        // Then: ROE = 当期純利益 / 純資産 × 100
        // 当期純利益 = 2,000,000
        // 純資産 = 5,000,000
        // ROE = 2,000,000 / 5,000,000 × 100 = 40%
        ratios.Roe.Should().Be(40.00m);
    }
}
