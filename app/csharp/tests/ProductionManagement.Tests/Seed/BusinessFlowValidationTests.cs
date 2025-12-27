using Dapper;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;
using Npgsql;
using ProductionManagement.Infrastructure.Seed;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Seed;

/// <summary>
/// 業務フロー検証テスト
/// Seed データを使用して、業務フローの整合性を検証する
/// </summary>
[Collection("Database")]
public class BusinessFlowValidationTests
{
    private readonly PostgresFixture _fixture;

    public BusinessFlowValidationTests(PostgresFixture fixture)
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

    #region MRP検証テスト

    [Fact]
    [Trait("Category", "Integration")]
    public async Task 製造オーダの計画数量が正の値である()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);
        await seedDataService.SeedAllAsync();

        // Act
        var invalidOrders = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "オーダ情報"
            WHERE "計画数量" <= 0
            """);

        // Assert
        invalidOrders.Should().Be(0, "オーダの計画数量は正の値でなければなりません");
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task 製造オーダから所要量が正しく計算される()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);
        await seedDataService.SeedAllAsync();

        // Act - MO-2025-001のオーダを取得
        var order = await connection.QuerySingleOrDefaultAsync<dynamic>("""
            SELECT "オーダNO", "品目コード", "計画数量"
            FROM "オーダ情報"
            WHERE "オーダNO" = 'MO-2025-001'
            """);

        // Assert
        ((object?)order).Should().NotBeNull("オーダ MO-2025-001 が見つかりません");
        ((decimal)order.計画数量).Should().Be(100m);

        // BOM 展開で必要量を計算
        var boms = await connection.QueryAsync<dynamic>("""
            SELECT "子品目コード", "必要数量"
            FROM "部品構成表"
            WHERE "親品目コード" = @ItemCode
            """, new { ItemCode = (string)order.品目コード });

        boms.Should().NotBeEmpty("製品には部品構成が必要です");

        foreach (var bom in boms)
        {
            var requiredQty = (decimal)order.計画数量 * (decimal)bom.必要数量;
            requiredQty.Should().BePositive($"必要量は正の値でなければなりません: {bom.子品目コード}");
        }
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task オーダの着手予定日が納期より前である()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);
        await seedDataService.SeedAllAsync();

        // Act
        var invalidOrders = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "オーダ情報"
            WHERE "着手予定日" > "納期"
            """);

        // Assert
        invalidOrders.Should().Be(0, "着手予定日は納期より前でなければなりません");
    }

    #endregion

    #region 購買フロー検証テスト

    [Fact]
    [Trait("Category", "Integration")]
    public async Task 発注から入荷までのフローが正しく動作する()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);
        await seedDataService.SeedAllAsync();

        // Act
        var purchaseOrder = await connection.QuerySingleOrDefaultAsync<dynamic>("""
            SELECT "発注番号", "ステータス"::TEXT AS "ステータス"
            FROM "発注データ"
            WHERE "発注番号" = 'PUR-2025-001'
            """);

        // Assert
        ((object?)purchaseOrder).Should().NotBeNull("発注 PUR-2025-001 が見つかりません");
        ((string)purchaseOrder.ステータス).Should().Be("発注済");

        var details = await connection.QueryAsync<dynamic>("""
            SELECT "発注数量", "発注単価"
            FROM "発注明細データ"
            WHERE "発注番号" = @OrderNumber
            """, new { OrderNumber = (string)purchaseOrder.発注番号 });

        details.Should().NotBeEmpty();

        foreach (var detail in details)
        {
            ((decimal)detail.発注数量).Should().BePositive();
            ((decimal)detail.発注単価).Should().BePositive();
        }
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task すべての発注明細が発注データに紐づいている()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);
        await seedDataService.SeedAllAsync();

        // Act
        var orphanedDetails = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "発注明細データ" d
            WHERE NOT EXISTS (
                SELECT 1 FROM "発注データ" h
                WHERE h."発注番号" = d."発注番号"
            )
            """);

        // Assert
        orphanedDetails.Should().Be(0, "すべての発注明細は発注データに紐づいている必要があります");
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task 発注金額が正しく計算されている()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);
        await seedDataService.SeedAllAsync();

        // Act
        var details = await connection.QueryAsync<dynamic>("""
            SELECT "発注番号", "発注数量", "発注単価", "発注金額"
            FROM "発注明細データ"
            """);

        // Assert
        foreach (var detail in details)
        {
            var expectedAmount = (decimal)detail.発注数量 * (decimal)detail.発注単価;
            ((decimal)detail.発注金額).Should().Be(expectedAmount,
                $"発注 {detail.発注番号} の発注金額が正しく計算されていません");
        }
    }

    #endregion

    #region 製造フロー検証テスト

    [Fact]
    [Trait("Category", "Integration")]
    public async Task 作業指示に対する完成実績が正しく記録される()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);
        await seedDataService.SeedAllAsync();

        // Act
        var workOrder = await connection.QuerySingleOrDefaultAsync<dynamic>("""
            SELECT "作業指示番号", "ステータス"::TEXT AS "ステータス"
            FROM "作業指示データ"
            WHERE "作業指示番号" = 'WO-2025-002'
            """);

        // Assert
        ((object?)workOrder).Should().NotBeNull("作業指示 WO-2025-002 が見つかりません");
        ((string)workOrder.ステータス).Should().Be("完了");

        var completionRecords = await connection.QueryAsync<dynamic>("""
            SELECT "完成実績番号", "完成数量", "良品数"
            FROM "完成実績データ"
            WHERE "作業指示番号" = @WorkOrderNumber
            """, new { WorkOrderNumber = (string)workOrder.作業指示番号 });

        completionRecords.Should().NotBeEmpty();

        foreach (var record in completionRecords)
        {
            var completedQty = (decimal)record.完成数量;
            var goodQty = (decimal)record.良品数;

            completedQty.Should().BePositive();
            goodQty.Should().BeGreaterThanOrEqualTo(0);
            goodQty.Should().BeLessThanOrEqualTo(completedQty, "良品数は完成数量以下でなければなりません");

            // 良品率が90%以上を期待
            var yieldRate = goodQty / completedQty * 100m;
            yieldRate.Should().BeGreaterThan(90m, $"良品率は90%以上を期待: {record.完成実績番号}");
        }
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task 工数実績が正しく記録される()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);
        await seedDataService.SeedAllAsync();

        // Act
        var laborRecords = await connection.QueryAsync<dynamic>("""
            SELECT "工数実績番号", "工数"
            FROM "工数実績データ"
            """);

        // Assert
        laborRecords.Should().NotBeEmpty();

        var totalLaborHours = 0m;
        foreach (var record in laborRecords)
        {
            var workTime = (decimal)record.工数;
            workTime.Should().BePositive($"工数は正の値でなければなりません: {record.工数実績番号}");
            totalLaborHours += workTime;
        }

        totalLaborHours.Should().BePositive("合計工数は0より大きくなければなりません");
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task すべての完成実績が作業指示に紐づいている()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);
        await seedDataService.SeedAllAsync();

        // Act
        var orphanedRecords = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "完成実績データ" c
            WHERE NOT EXISTS (
                SELECT 1 FROM "作業指示データ" w
                WHERE w."作業指示番号" = c."作業指示番号"
            )
            """);

        // Assert
        orphanedRecords.Should().Be(0, "すべての完成実績は作業指示に紐づいている必要があります");
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task すべての工数実績が作業指示に紐づいている()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var seedDataService = await CreateSeedDataServiceAsync(connection);
        await seedDataService.SeedAllAsync();

        // Act
        var orphanedRecords = await connection.ExecuteScalarAsync<int>("""
            SELECT COUNT(*)
            FROM "工数実績データ" l
            WHERE NOT EXISTS (
                SELECT 1 FROM "作業指示データ" w
                WHERE w."作業指示番号" = l."作業指示番号"
            )
            """);

        // Assert
        orphanedRecords.Should().Be(0, "すべての工数実績は作業指示に紐づいている必要があります");
    }

    #endregion
}
