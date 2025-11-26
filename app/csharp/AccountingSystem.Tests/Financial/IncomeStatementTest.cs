using System.Data;
using AccountingSystem.Application.Services;
using AccountingSystem.Infrastructure.Repositories;
using Dapper;
using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Financial;

/// <summary>
/// 損益計算書生成のテスト
/// </summary>
public class IncomeStatementTest : DatabaseTestBase
{
    static IncomeStatementTest()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    private async Task SetUpTestDataAsync(NpgsqlConnection connection)
    {
        // テストデータ：勘定科目マスタ（損益計算書科目）
        // 取引要素区分: 1=資産, 2=負債, 3=純資産, 4=収益, 5=費用
        // 費用区分: 1=売上原価, 2=販管費
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分"", ""費用区分""
            ) VALUES
            ('4110', '売上高', '収益', 'P', '4', NULL),
            ('5110', '売上原価', '費用', 'P', '5', '1'),
            ('6110', '給与手当', '費用', 'P', '5', '2'),
            ('6210', '福利厚生費', '費用', 'P', '5', '2'),
            ('6310', '旅費交通費', '費用', 'P', '5', '2')
            ON CONFLICT (""勘定科目コード"") DO NOTHING
        ");

        // テストデータ：日次残高（2024年1月の期間データ）
        var fromDate = new DateOnly(2024, 1, 1);
        var toDate = new DateOnly(2024, 1, 31);

        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES
            -- 売上高（貸方）: 10,000,000
            (@FromDate, '4110', '', '', '', 0, 0, 10000000),
            -- 売上原価（借方）: 6,000,000
            (@FromDate, '5110', '', '', '', 0, 6000000, 0),
            -- 給与手当（借方）: 1,500,000
            (@ToDate, '6110', '', '', '', 0, 1500000, 0),
            -- 福利厚生費（借方）: 200,000
            (@ToDate, '6210', '', '', '', 0, 200000, 0),
            -- 旅費交通費（借方）: 300,000
            (@ToDate, '6310', '', '', '', 0, 300000, 0)
        ", new { FromDate = fromDate, ToDate = toDate });
    }

    [Fact(DisplayName = "損益計算書を生成できる")]
    public async Task Test_損益計算書を生成できる()
    {
        // Given: 2024年1月のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var fromDate = new DateOnly(2024, 1, 1);
        var toDate = new DateOnly(2024, 1, 31);

        // When: 損益計算書を生成
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, toDate);

        // Then: 損益計算書のプロパティが存在する
        incomeStatement.Should().NotBeNull();
        incomeStatement.FromDate.Should().Be(fromDate);
        incomeStatement.ToDate.Should().Be(toDate);
        incomeStatement.Revenues.Should().NotBeEmpty();
        incomeStatement.Expenses.Should().NotBeEmpty();
        incomeStatement.TotalRevenues.Should().BeGreaterThan(0m);
        incomeStatement.TotalExpenses.Should().BeGreaterThan(0m);
    }

    [Fact(DisplayName = "売上総利益が正しく計算されている")]
    public async Task Test_売上総利益が正しく計算されている()
    {
        // Given: 2024年1月のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var fromDate = new DateOnly(2024, 1, 1);
        var toDate = new DateOnly(2024, 1, 31);

        // When: 損益計算書を生成
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, toDate);

        // Then: 売上総利益 = 売上高 - 売上原価 = 10,000,000 - 6,000,000 = 4,000,000
        incomeStatement.GrossProfit.Should().Be(4000000m);
    }

    [Fact(DisplayName = "営業利益が正しく計算されている")]
    public async Task Test_営業利益が正しく計算されている()
    {
        // Given: 2024年1月のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var fromDate = new DateOnly(2024, 1, 1);
        var toDate = new DateOnly(2024, 1, 31);

        // When: 損益計算書を生成
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, toDate);

        // Then: 営業利益 = 売上総利益 - 販管費
        // 販管費 = 給与手当1,500,000 + 福利厚生費200,000 + 旅費交通費300,000 = 2,000,000
        // 営業利益 = 4,000,000 - 2,000,000 = 2,000,000
        incomeStatement.OperatingIncome.Should().Be(2000000m);
    }

    [Fact(DisplayName = "当期純利益が正しく計算されている")]
    public async Task Test_当期純利益が正しく計算されている()
    {
        // Given: 2024年1月のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var fromDate = new DateOnly(2024, 1, 1);
        var toDate = new DateOnly(2024, 1, 31);

        // When: 損益計算書を生成
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, toDate);

        // Then: 当期純利益 = 総収益 - 総費用
        // 総収益 = 10,000,000
        // 総費用 = 6,000,000 + 1,500,000 + 200,000 + 300,000 = 8,000,000
        // 当期純利益 = 10,000,000 - 8,000,000 = 2,000,000
        incomeStatement.NetIncome.Should().Be(2000000m);
        incomeStatement.TotalRevenues.Should().Be(10000000m);
        incomeStatement.TotalExpenses.Should().Be(8000000m);
    }

    [Fact(DisplayName = "収益項目が正しく分類されている")]
    public async Task Test_収益項目が正しく分類されている()
    {
        // Given: 2024年1月のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var fromDate = new DateOnly(2024, 1, 1);
        var toDate = new DateOnly(2024, 1, 31);

        // When: 損益計算書を生成
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, toDate);

        // Then: 収益項目が存在する
        var revenues = incomeStatement.Revenues;
        revenues.Should().HaveCount(1);
        revenues[0].AccountCode.Should().Be("4110");
        revenues[0].AccountName.Should().Be("売上高");
        revenues[0].Amount.Should().Be(10000000m);
    }

    [Fact(DisplayName = "費用項目が正しく分類されている")]
    public async Task Test_費用項目が正しく分類されている()
    {
        // Given: 2024年1月のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var fromDate = new DateOnly(2024, 1, 1);
        var toDate = new DateOnly(2024, 1, 31);

        // When: 損益計算書を生成
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, toDate);

        // Then: 費用項目が存在する
        var expenses = incomeStatement.Expenses;
        expenses.Should().HaveCount(4);

        // 売上原価
        var costOfSales = expenses.First(e => e.AccountCode == "5110");
        costOfSales.AccountName.Should().Be("売上原価");
        costOfSales.Amount.Should().Be(6000000m);

        // 販管費（給与手当）
        var salary = expenses.First(e => e.AccountCode == "6110");
        salary.AccountName.Should().Be("給与手当");
        salary.Amount.Should().Be(1500000m);
    }

    [Fact(DisplayName = "対売上比率が計算されている")]
    public async Task Test_対売上比率が計算されている()
    {
        // Given: 2024年1月のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var fromDate = new DateOnly(2024, 1, 1);
        var toDate = new DateOnly(2024, 1, 31);

        // When: 損益計算書を生成
        var incomeStatement = await service.GenerateIncomeStatementAsync(fromDate, toDate);

        // Then: 各項目の対売上比率が計算されている
        foreach (var revenue in incomeStatement.Revenues)
        {
            revenue.Percentage.Should().BeGreaterThanOrEqualTo(0m);
            revenue.Percentage.Should().BeLessThanOrEqualTo(100m);
        }

        foreach (var expense in incomeStatement.Expenses)
        {
            expense.Percentage.Should().BeGreaterThanOrEqualTo(0m);
            expense.Percentage.Should().BeLessThanOrEqualTo(100m);
        }

        // 売上原価の対売上比率 = 6,000,000 / 10,000,000 × 100 = 60%
        var costOfSales = incomeStatement.Expenses.First(e => e.AccountCode == "5110");
        costOfSales.Percentage.Should().Be(60.00m);
    }
}
