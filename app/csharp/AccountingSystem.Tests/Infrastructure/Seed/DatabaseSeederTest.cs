using AccountingSystem.Infrastructure.Seed;
using Dapper;
using FluentAssertions;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Moq;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Infrastructure.Seed;

/// <summary>
/// DatabaseSeeder の統合テスト
/// </summary>
public class DatabaseSeederTest : DatabaseTestBase
{
    [Fact]
    public async Task SeedDatabaseAsync_勘定科目マスタが投入される()
    {
        // Arrange
        var configuration = CreateConfiguration();
        var logger = new Mock<ILogger<DatabaseSeeder>>();
        var seeder = new DatabaseSeeder(configuration, logger.Object);

        // Act
        await seeder.SeedDatabaseAsync();

        // Assert
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        var count = await connection.ExecuteScalarAsync<int>(
            @"SELECT COUNT(*) FROM ""勘定科目マスタ""");

        count.Should().Be(41); // AccountingSeedData に定義された勘定科目数
    }

    [Fact]
    public async Task SeedDatabaseAsync_勘定科目構成マスタが投入される()
    {
        // Arrange
        var configuration = CreateConfiguration();
        var logger = new Mock<ILogger<DatabaseSeeder>>();
        var seeder = new DatabaseSeeder(configuration, logger.Object);

        // Act
        await seeder.SeedDatabaseAsync();

        // Assert
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        var count = await connection.ExecuteScalarAsync<int>(
            @"SELECT COUNT(*) FROM ""勘定科目構成マスタ""");

        count.Should().Be(41); // AccountingSeedData に定義された構成データ数
    }

    [Fact]
    public async Task SeedDatabaseAsync_日次勘定科目残高が投入される()
    {
        // Arrange
        var configuration = CreateConfiguration();
        var logger = new Mock<ILogger<DatabaseSeeder>>();
        var seeder = new DatabaseSeeder(configuration, logger.Object);

        // Act
        await seeder.SeedDatabaseAsync();

        // Assert
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        var count = await connection.ExecuteScalarAsync<int>(
            @"SELECT COUNT(*) FROM ""日次勘定科目残高""");

        // FY2021: 13件 + FY2022: 14件 = 27件
        count.Should().Be(27);
    }

    [Fact]
    public async Task SeedDatabaseAsync_月次勘定科目残高が投入される()
    {
        // Arrange
        var configuration = CreateConfiguration();
        var logger = new Mock<ILogger<DatabaseSeeder>>();
        var seeder = new DatabaseSeeder(configuration, logger.Object);

        // Act
        await seeder.SeedDatabaseAsync();

        // Assert
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        var count = await connection.ExecuteScalarAsync<int>(
            @"SELECT COUNT(*) FROM ""月次勘定科目残高""");

        // FY2021: 26件 + FY2022: 27件 = 53件（集計科目・純資産・損益計算書項目を含む）
        count.Should().Be(53);
    }

    [Fact]
    public async Task SeedDatabaseAsync_冪等性_既存データがある場合はスキップされる()
    {
        // Arrange
        var configuration = CreateConfiguration();
        var logger = new Mock<ILogger<DatabaseSeeder>>();
        var seeder = new DatabaseSeeder(configuration, logger.Object);

        // 1回目の Seed 実行
        await seeder.SeedDatabaseAsync();

        // Act: 2回目の Seed 実行
        await seeder.SeedDatabaseAsync();

        // Assert: データ件数が変わらない（重複投入されていない）
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        var accountCount = await connection.ExecuteScalarAsync<int>(
            @"SELECT COUNT(*) FROM ""勘定科目マスタ""");
        accountCount.Should().Be(41);

        var structureCount = await connection.ExecuteScalarAsync<int>(
            @"SELECT COUNT(*) FROM ""勘定科目構成マスタ""");
        structureCount.Should().Be(41);
    }

    [Fact]
    public async Task SeedDatabaseAsync_資産勘定科目のデータが正しく投入される()
    {
        // Arrange
        var configuration = CreateConfiguration();
        var logger = new Mock<ILogger<DatabaseSeeder>>();
        var seeder = new DatabaseSeeder(configuration, logger.Object);

        // Act
        await seeder.SeedDatabaseAsync();

        // Assert
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // Enum 型は ::text にキャストして取得
        var account = await connection.QuerySingleOrDefaultAsync<dynamic>(
            @"SELECT ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別""::text AS ""勘定科目種別"",
                     ""合計科目"", ""BSPL区分""
              FROM ""勘定科目マスタ"" WHERE ""勘定科目コード"" = '1'");

        Assert.NotNull(account);
        ((string)account.勘定科目名).Should().Be("資産");
        ((string)account.勘定科目種別).Should().Be("資産"); // PostgreSQL enum 値
        ((bool)account.合計科目).Should().BeTrue(); // Level 1 は合計科目
        ((string)account.BSPL区分).Should().Be("B"); // B: 貸借対照表
    }

    [Fact]
    public async Task SeedDatabaseAsync_現金預金残高が正しく投入される()
    {
        // Arrange
        var configuration = CreateConfiguration();
        var logger = new Mock<ILogger<DatabaseSeeder>>();
        var seeder = new DatabaseSeeder(configuration, logger.Object);

        // Act
        await seeder.SeedDatabaseAsync();

        // Assert
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // FY2022 の現金預金（111）残高を確認
        var balance = await connection.QuerySingleOrDefaultAsync<dynamic>(
            @"SELECT * FROM ""月次勘定科目残高""
              WHERE ""勘定科目コード"" = '111' AND ""決算期"" = 2022");

        Assert.NotNull(balance);
        ((decimal)balance.借方金額).Should().Be(1_133_270m); // D社の令和4年度現金預金
    }

    [Fact]
    public async Task SeedDatabaseAsync_勘定科目構成マスタの階層パスが正しく設定される()
    {
        // Arrange
        var configuration = CreateConfiguration();
        var logger = new Mock<ILogger<DatabaseSeeder>>();
        var seeder = new DatabaseSeeder(configuration, logger.Object);

        // Act
        await seeder.SeedDatabaseAsync();

        // Assert
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 建物及び構築物（1211）の階層パスを確認
        var structure = await connection.QuerySingleOrDefaultAsync<dynamic>(
            @"SELECT * FROM ""勘定科目構成マスタ"" WHERE ""勘定科目コード"" = '1211'");

        Assert.NotNull(structure);
        ((string)structure.勘定科目パス).Should().Be("1~12~121~1211");
        ((int)structure.階層レベル).Should().Be(4);
        ((string)structure.親科目コード).Should().Be("121");
    }

    private IConfiguration CreateConfiguration()
    {
        var inMemorySettings = new Dictionary<string, string?>
        {
            { "ConnectionStrings:DefaultConnection", ConnectionString },
            { "Seed:EnableOnStartup", "false" }
        };

        return new ConfigurationBuilder()
            .AddInMemoryCollection(inMemorySettings)
            .Build();
    }
}
