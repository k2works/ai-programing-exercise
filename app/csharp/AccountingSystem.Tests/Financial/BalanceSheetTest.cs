using System.Data;
using AccountingSystem.Application.Services;
using AccountingSystem.Infrastructure.Repositories;
using Dapper;
using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Financial;

/// <summary>
/// 貸借対照表生成のテスト
/// </summary>
public class BalanceSheetTest : DatabaseTestBase
{
    static BalanceSheetTest()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    private async Task SetUpTestDataAsync(NpgsqlConnection connection)
    {
        // テストデータ：勘定科目マスタ（貸借対照表科目）
        // 取引要素区分: 1=資産, 2=負債, 3=純資産, 4=収益, 5=費用
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

        // テストデータ：日次残高（2024-01-31時点）
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

    [Fact(DisplayName = "貸借対照表を生成できる")]
    public async Task Test_貸借対照表を生成できる()
    {
        // Given: 2024-01-31時点のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);

        // When: 貸借対照表を生成
        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);

        // Then: 貸借対照表のプロパティが存在する
        balanceSheet.Should().NotBeNull();
        balanceSheet.AsOfDate.Should().Be(asOfDate);
        balanceSheet.Assets.Should().NotBeEmpty();
        balanceSheet.Liabilities.Should().NotBeEmpty();
        balanceSheet.Equity.Should().NotBeEmpty();
        balanceSheet.TotalAssets.Should().BeGreaterThan(0m);
        balanceSheet.TotalLiabilities.Should().BeGreaterThan(0m);
        balanceSheet.TotalEquity.Should().BeGreaterThan(0m);
        balanceSheet.TotalLiabilitiesAndEquity.Should().BeGreaterThan(0m);
    }

    [Fact(DisplayName = "貸借平均の原則が成立している")]
    public async Task Test_貸借平均の原則が成立している()
    {
        // Given: 2024-01-31時点のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);

        // When: 貸借対照表を生成
        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);

        // Then: 資産 = 負債 + 純資産
        var expectedTotal = balanceSheet.TotalLiabilities + balanceSheet.TotalEquity;
        balanceSheet.TotalAssets.Should().Be(expectedTotal);
        balanceSheet.TotalAssets.Should().Be(balanceSheet.TotalLiabilitiesAndEquity);

        // 具体的な金額の検証
        // 資産: 普通預金8,000,000 + 建物2,000,000 = 10,000,000
        balanceSheet.TotalAssets.Should().Be(10000000m);

        // 負債: 買掛金500,000 + 長期借入金4,500,000 = 5,000,000
        balanceSheet.TotalLiabilities.Should().Be(5000000m);

        // 純資産: 資本金5,000,000
        balanceSheet.TotalEquity.Should().Be(5000000m);
    }

    [Fact(DisplayName = "資産項目が正しく分類されている")]
    public async Task Test_資産項目が正しく分類されている()
    {
        // Given: 2024-01-31時点のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);

        // When: 貸借対照表を生成
        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);

        // Then: 資産項目が存在する
        var assets = balanceSheet.Assets;
        assets.Should().HaveCount(2);

        // 流動資産（11で始まる勘定科目）
        var currentAssets = assets.Where(asset => asset.AccountCode.StartsWith("11")).ToList();
        currentAssets.Should().HaveCount(1);
        currentAssets[0].AccountCode.Should().Be("1110");
        currentAssets[0].AccountName.Should().Be("普通預金");
        currentAssets[0].Balance.Should().Be(8000000m);

        // 固定資産（14で始まる勘定科目）
        var fixedAssets = assets.Where(asset => asset.AccountCode.StartsWith("14")).ToList();
        fixedAssets.Should().HaveCount(1);
        fixedAssets[0].AccountCode.Should().Be("1410");
        fixedAssets[0].AccountName.Should().Be("建物");
        fixedAssets[0].Balance.Should().Be(2000000m);
    }

    [Fact(DisplayName = "負債項目が正しく分類されている")]
    public async Task Test_負債項目が正しく分類されている()
    {
        // Given: 2024-01-31時点のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);

        // When: 貸借対照表を生成
        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);

        // Then: 負債項目が存在する
        var liabilities = balanceSheet.Liabilities;
        liabilities.Should().HaveCount(2);

        // 流動負債（21で始まる勘定科目）
        var currentLiabilities = liabilities.Where(l => l.AccountCode.StartsWith("21")).ToList();
        currentLiabilities.Should().HaveCount(1);
        currentLiabilities[0].AccountCode.Should().Be("2110");
        currentLiabilities[0].AccountName.Should().Be("買掛金");
        currentLiabilities[0].Balance.Should().Be(500000m);

        // 固定負債（25で始まる勘定科目）
        var longTermLiabilities = liabilities.Where(l => l.AccountCode.StartsWith("25")).ToList();
        longTermLiabilities.Should().HaveCount(1);
        longTermLiabilities[0].AccountCode.Should().Be("2510");
        longTermLiabilities[0].AccountName.Should().Be("長期借入金");
        longTermLiabilities[0].Balance.Should().Be(4500000m);
    }

    [Fact(DisplayName = "純資産項目が正しく分類されている")]
    public async Task Test_純資産項目が正しく分類されている()
    {
        // Given: 2024-01-31時点のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);

        // When: 貸借対照表を生成
        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);

        // Then: 純資産項目が存在する
        var equity = balanceSheet.Equity;
        equity.Should().HaveCount(1);
        equity[0].AccountCode.Should().Be("3110");
        equity[0].AccountName.Should().Be("資本金");
        equity[0].Balance.Should().Be(5000000m);
    }

    [Fact(DisplayName = "構成比率が計算されている")]
    public async Task Test_構成比率が計算されている()
    {
        // Given: 2024-01-31時点のデータ
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        await SetUpTestDataAsync(connection);

        var service = new FinancialStatementService(ConnectionString);
        var asOfDate = new DateOnly(2024, 1, 31);

        // When: 貸借対照表を生成
        var balanceSheet = await service.GenerateBalanceSheetAsync(asOfDate);

        // Then: 各項目の構成比率が存在し、0〜100の範囲内
        foreach (var asset in balanceSheet.Assets)
        {
            asset.Percentage.Should().BeGreaterThanOrEqualTo(0m);
            asset.Percentage.Should().BeLessThanOrEqualTo(100m);
        }

        foreach (var liability in balanceSheet.Liabilities)
        {
            liability.Percentage.Should().BeGreaterThanOrEqualTo(0m);
            liability.Percentage.Should().BeLessThanOrEqualTo(100m);
        }

        foreach (var eq in balanceSheet.Equity)
        {
            eq.Percentage.Should().BeGreaterThanOrEqualTo(0m);
            eq.Percentage.Should().BeLessThanOrEqualTo(100m);
        }

        // 普通預金の構成比率 = 8,000,000 / 10,000,000 × 100 = 80%
        var ordinaryDeposit = balanceSheet.Assets.First(a => a.AccountCode == "1110");
        ordinaryDeposit.Percentage.Should().Be(80.00m);
    }
}
