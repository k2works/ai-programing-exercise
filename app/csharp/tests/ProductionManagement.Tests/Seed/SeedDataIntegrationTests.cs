using Dapper;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;
using Npgsql;
using ProductionManagement.Infrastructure.Seed;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Seed;

/// <summary>
/// Seedデータ統合テスト
/// </summary>
[Collection("Database")]
public class SeedDataIntegrationTests
{
    private readonly PostgresFixture _fixture;

    public SeedDataIntegrationTests(PostgresFixture fixture)
    {
        _fixture = fixture;
    }

    private async Task<SeedDataService> CreateSeedDataServiceAsync(NpgsqlConnection connection)
    {
        var loggerFactory = NullLoggerFactory.Instance;
        var masterSeeder = new MasterDataSeeder(
            loggerFactory.CreateLogger<MasterDataSeeder>(),
            connection);
        var transactionSeeder = new TransactionDataSeeder(
            loggerFactory.CreateLogger<TransactionDataSeeder>(),
            connection);

        return new SeedDataService(
            loggerFactory.CreateLogger<SeedDataService>(),
            connection,
            masterSeeder,
            transactionSeeder);
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task Seedデータを投入できる()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act
        await seedDataService.SeedAllAsync();

        // Assert - マスタデータの件数確認
        var unitCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "単位マスタ" """);
        unitCount.Should().Be(5);

        var locationCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "場所マスタ" """);
        locationCount.Should().Be(8);

        var supplierCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "取引先マスタ" """);
        supplierCount.Should().Be(10);

        var departmentCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "部門マスタ" """);
        departmentCount.Should().Be(7);

        var processCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "工程マスタ" """);
        processCount.Should().Be(12);

        var itemCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "品目マスタ" """);
        itemCount.Should().Be(25);

        var bomCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "部品構成表" """);
        bomCount.Should().Be(19);

        var routingCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "工程表" """);
        routingCount.Should().Be(24);

        var employeeCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "担当者マスタ" """);
        employeeCount.Should().Be(12);

        var defectCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "欠点マスタ" """);
        defectCount.Should().Be(6);
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task トランザクションデータを投入できる()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act
        await seedDataService.SeedAllAsync();

        // Assert - トランザクションデータの件数確認
        var stockCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "在庫情報" """);
        stockCount.Should().Be(17);

        var orderCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "オーダ情報" """);
        orderCount.Should().Be(9);

        var purchaseOrderCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "発注データ" """);
        purchaseOrderCount.Should().Be(3);

        var workOrderCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "作業指示データ" """);
        workOrderCount.Should().Be(2);

        var completionCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "完成実績データ" """);
        completionCount.Should().Be(2);

        var laborCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "工数実績データ" """);
        laborCount.Should().Be(3);
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task すべての品目が単位を持つ()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act
        await seedDataService.SeedAllAsync();

        // Assert
        var itemsWithoutUnit = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "品目マスタ" i
            WHERE NOT EXISTS (
                SELECT 1 FROM "単位マスタ" u
                WHERE u."単位コード" = i."単位コード"
            )
            """);
        itemsWithoutUnit.Should().Be(0);
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task すべての担当者が部門を持つ()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act
        await seedDataService.SeedAllAsync();

        // Assert
        var employeesWithoutDept = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "担当者マスタ" e
            WHERE NOT EXISTS (
                SELECT 1 FROM "部門マスタ" d
                WHERE d."部門コード" = e."部門コード"
            )
            """);
        employeesWithoutDept.Should().Be(0);
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task すべての工程表が工程マスタを参照する()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act
        await seedDataService.SeedAllAsync();

        // Assert
        var routingsWithoutProcess = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "工程表" r
            WHERE NOT EXISTS (
                SELECT 1 FROM "工程マスタ" p
                WHERE p."工程コード" = r."工程コード"
            )
            """);
        routingsWithoutProcess.Should().Be(0);
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task すべての在庫が場所マスタを参照する()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act
        await seedDataService.SeedAllAsync();

        // Assert
        var stocksWithoutLocation = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "在庫情報" s
            WHERE NOT EXISTS (
                SELECT 1 FROM "場所マスタ" l
                WHERE l."場所コード" = s."場所コード"
            )
            """);
        stocksWithoutLocation.Should().Be(0);
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task BOM展開でギアボックスアセンブリの部品構成を確認できる()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act
        await seedDataService.SeedAllAsync();

        // Assert - ギアボックスアセンブリ(PROD-B001)の直接子部品数
        var childCount = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "部品構成表"
            WHERE "親品目コード" = 'PROD-B001'
            """);
        childCount.Should().Be(8);
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task 重複実行しても正常に動作する()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act - 2回実行
        await seedDataService.SeedAllAsync();
        await seedDataService.SeedAllAsync();

        // Assert - データが正常に再投入される
        var itemCount = await connection.ExecuteScalarAsync<int>(
            """SELECT COUNT(*) FROM "品目マスタ" """);
        itemCount.Should().Be(25);
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task BOMに循環参照がない()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act
        await seedDataService.SeedAllAsync();

        // Assert - 親品目コードと子品目コードが同じレコードがないこと
        var selfReferencingBoms = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "部品構成表"
            WHERE "親品目コード" = "子品目コード"
            """);
        selfReferencingBoms.Should().Be(0, "BOMに循環参照（自己参照）があってはいけません");
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task 在庫数量が0以上である()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act
        await seedDataService.SeedAllAsync();

        // Assert
        var negativeStocks = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "在庫情報"
            WHERE "在庫数量" < 0
            """);
        negativeStocks.Should().Be(0, "在庫数量が負の値であってはいけません");
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task BOMの必要数量が正である()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act
        await seedDataService.SeedAllAsync();

        // Assert
        var invalidBoms = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "部品構成表"
            WHERE "必要数量" <= 0
            """);
        invalidBoms.Should().Be(0, "BOMの必要数量は正の値でなければなりません");
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task すべての在庫が品目マスタを参照する()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);

        // Act
        await seedDataService.SeedAllAsync();

        // Assert
        var stocksWithoutItem = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "在庫情報" s
            WHERE NOT EXISTS (
                SELECT 1 FROM "品目マスタ" i
                WHERE i."品目コード" = s."品目コード"
            )
            """);
        stocksWithoutItem.Should().Be(0);
    }
}
