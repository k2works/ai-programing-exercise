using System.Data;
using AccountingSystem.Application.Services;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using Dapper;
using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Application.Services;

/// <summary>
/// 財務諸表生成サービスのテスト
/// FinancialStatementService の全メソッドをテスト
/// </summary>
public class FinancialStatementServiceTest : DatabaseTestBase
{
    static FinancialStatementServiceTest()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    private async Task SetUpBalanceSheetDataAsync(NpgsqlConnection connection)
    {
        // 貸借対照表用テストデータ
        // 取引要素区分: 1=資産, 2=負債, 3=純資産
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES
            ('1110', '普通預金', '資産', 'B', '1'),
            ('1410', '建物', '資産', 'B', '1'),
            ('2110', '買掛金', '負債', 'B', '2'),
            ('2510', '長期借入金', '負債', 'B', '2'),
            ('3110', '資本金', '純資産', 'B', '3')
            ON CONFLICT (""勘定科目コード"") DO NOTHING
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
            (@AsOfDate, '3110', '', '', '', 0, 0, 5000000)
        ", new { AsOfDate = asOfDate });
    }

    private async Task SetUpIncomeStatementDataAsync(NpgsqlConnection connection)
    {
        // 損益計算書用テストデータ
        // 取引要素区分: 4=収益, 5=費用
        // 費用区分: 1=売上原価, 2=販管費
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分"", ""費用区分""
            ) VALUES
            ('4110', '売上高', '収益', 'P', '4', NULL),
            ('5110', '売上原価', '費用', 'P', '5', '1'),
            ('6110', '給与手当', '費用', 'P', '5', '2'),
            ('6210', '福利厚生費', '費用', 'P', '5', '2')
            ON CONFLICT (""勘定科目コード"") DO NOTHING
        ");

        var entryDate = new DateOnly(2024, 1, 15);
        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES
            (@EntryDate, '4110', '', '', '', 0, 0, 10000000),
            (@EntryDate, '5110', '', '', '', 0, 6000000, 0),
            (@EntryDate, '6110', '', '', '', 0, 1500000, 0),
            (@EntryDate, '6210', '', '', '', 0, 500000, 0)
        ", new { EntryDate = entryDate });
    }

    #region 貸借対照表生成テスト

    [Fact(DisplayName = "GenerateBalanceSheetAsync - 貸借対照表を生成できる")]
    public async Task GenerateBalanceSheetAsync_貸借対照表を生成できる()
    {
        // Given
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpBalanceSheetDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);

        // When
        var result = await service.GenerateBalanceSheetAsync(asOfDate);

        // Then
        result.Should().NotBeNull();
        result.AsOfDate.Should().Be(asOfDate);
        result.Assets.Should().NotBeEmpty();
        result.Liabilities.Should().NotBeEmpty();
        result.Equity.Should().NotBeEmpty();
    }

    [Fact(DisplayName = "GenerateBalanceSheetAsync - 貸借平均の原則が成立する")]
    public async Task GenerateBalanceSheetAsync_貸借平均の原則が成立する()
    {
        // Given
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpBalanceSheetDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);

        // When
        var result = await service.GenerateBalanceSheetAsync(asOfDate);

        // Then: 資産 = 負債 + 純資産
        result.TotalAssets.Should().Be(result.TotalLiabilitiesAndEquity);
        result.TotalAssets.Should().Be(10000000m);
        result.TotalLiabilities.Should().Be(5000000m);
        result.TotalEquity.Should().Be(5000000m);
    }

    [Fact(DisplayName = "GenerateBalanceSheetAsync - 構成比率が計算される")]
    public async Task GenerateBalanceSheetAsync_構成比率が計算される()
    {
        // Given
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpBalanceSheetDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);

        // When
        var result = await service.GenerateBalanceSheetAsync(asOfDate);

        // Then: 普通預金 8,000,000 / 10,000,000 = 80%
        var deposit = result.Assets.First(a => a.AccountCode == "1110");
        deposit.Percentage.Should().Be(80.00m);
    }

    #endregion

    #region 損益計算書生成テスト

    [Fact(DisplayName = "GenerateIncomeStatementAsync - 損益計算書を生成できる")]
    public async Task GenerateIncomeStatementAsync_損益計算書を生成できる()
    {
        // Given
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpIncomeStatementDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var fromDate = new DateOnly(2024, 1, 1);
        var toDate = new DateOnly(2024, 1, 31);

        // When
        var result = await service.GenerateIncomeStatementAsync(fromDate, toDate);

        // Then
        result.Should().NotBeNull();
        result.FromDate.Should().Be(fromDate);
        result.ToDate.Should().Be(toDate);
        result.Revenues.Should().NotBeEmpty();
        result.Expenses.Should().NotBeEmpty();
    }

    [Fact(DisplayName = "GenerateIncomeStatementAsync - 利益項目が正しく計算される")]
    public async Task GenerateIncomeStatementAsync_利益項目が正しく計算される()
    {
        // Given
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpIncomeStatementDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var fromDate = new DateOnly(2024, 1, 1);
        var toDate = new DateOnly(2024, 1, 31);

        // When
        var result = await service.GenerateIncomeStatementAsync(fromDate, toDate);

        // Then
        // 売上高: 10,000,000
        result.TotalRevenues.Should().Be(10000000m);
        // 総費用: 6,000,000 + 1,500,000 + 500,000 = 8,000,000
        result.TotalExpenses.Should().Be(8000000m);
        // 売上総利益: 10,000,000 - 6,000,000 = 4,000,000
        result.GrossProfit.Should().Be(4000000m);
        // 営業利益: 4,000,000 - 2,000,000 = 2,000,000
        result.OperatingIncome.Should().Be(2000000m);
        // 当期純利益: 10,000,000 - 8,000,000 = 2,000,000
        result.NetIncome.Should().Be(2000000m);
    }

    [Fact(DisplayName = "GenerateIncomeStatementAsync - 対売上比率が計算される")]
    public async Task GenerateIncomeStatementAsync_対売上比率が計算される()
    {
        // Given
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpIncomeStatementDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var fromDate = new DateOnly(2024, 1, 1);
        var toDate = new DateOnly(2024, 1, 31);

        // When
        var result = await service.GenerateIncomeStatementAsync(fromDate, toDate);

        // Then: 売上原価 6,000,000 / 10,000,000 = 60%
        var costOfSales = result.Expenses.First(e => e.AccountCode == "5110");
        costOfSales.Percentage.Should().Be(60.00m);
    }

    #endregion

    #region 財務指標計算テスト

    [Fact(DisplayName = "CalculateFinancialRatios - 財務指標を計算できる")]
    public async Task CalculateFinancialRatios_財務指標を計算できる()
    {
        // Given
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpBalanceSheetDataAsync(connection);
        await SetUpIncomeStatementDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);
        var fromDate = new DateOnly(2024, 1, 1);

        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, asOfDate);

        // When
        var result = service.CalculateFinancialRatios(balanceSheet, incomeStatement);

        // Then
        result.Should().NotBeNull();
        result.CurrentRatio.Should().BeGreaterThan(0m);
        result.EquityRatio.Should().BeGreaterThan(0m);
        result.GrossProfitMargin.Should().BeGreaterThan(0m);
        result.OperatingProfitMargin.Should().BeGreaterThan(0m);
        result.NetProfitMargin.Should().BeGreaterThan(0m);
        result.Roa.Should().BeGreaterThan(0m);
        result.Roe.Should().BeGreaterThan(0m);
    }

    [Fact(DisplayName = "CalculateFinancialRatios - 安全性指標が正しく計算される")]
    public async Task CalculateFinancialRatios_安全性指標が正しく計算される()
    {
        // Given
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpBalanceSheetDataAsync(connection);
        await SetUpIncomeStatementDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);
        var fromDate = new DateOnly(2024, 1, 1);

        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, asOfDate);

        // When
        var result = service.CalculateFinancialRatios(balanceSheet, incomeStatement);

        // Then
        // 流動比率: 流動資産8,000,000 / 流動負債500,000 = 1600%
        result.CurrentRatio.Should().Be(1600.00m);
        // 自己資本比率: 純資産5,000,000 / 総資産10,000,000 = 50%
        result.EquityRatio.Should().Be(50.00m);
    }

    [Fact(DisplayName = "CalculateFinancialRatios - 収益性指標が正しく計算される")]
    public async Task CalculateFinancialRatios_収益性指標が正しく計算される()
    {
        // Given
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpBalanceSheetDataAsync(connection);
        await SetUpIncomeStatementDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);
        var fromDate = new DateOnly(2024, 1, 1);

        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, asOfDate);

        // When
        var result = service.CalculateFinancialRatios(balanceSheet, incomeStatement);

        // Then
        // 売上総利益率: 4,000,000 / 10,000,000 = 40%
        result.GrossProfitMargin.Should().Be(40.00m);
        // 営業利益率: 2,000,000 / 10,000,000 = 20%
        result.OperatingProfitMargin.Should().Be(20.00m);
        // 当期純利益率: 2,000,000 / 10,000,000 = 20%
        result.NetProfitMargin.Should().Be(20.00m);
        // ROA: 2,000,000 / 10,000,000 = 20%
        result.Roa.Should().Be(20.00m);
        // ROE: 2,000,000 / 5,000,000 = 40%
        result.Roe.Should().Be(40.00m);
    }

    #endregion
}
