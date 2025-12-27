using System.Net;
using System.Net.Http.Json;
using FluentAssertions;
using Npgsql;
using ProductionManagement.Infrastructure.Rest.Dto;
using ProductionManagement.IntegrationTests.TestSetup;

namespace ProductionManagement.IntegrationTests.Controllers;

/// <summary>
/// 在庫照会 API の統合テスト
/// </summary>
[Collection("Postgres")]
public class InventoryControllerTests : IAsyncLifetime
{
    private readonly PostgresFixture _fixture;
    private readonly CustomWebApplicationFactory _factory;
    private readonly HttpClient _client;

    public InventoryControllerTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _factory = new CustomWebApplicationFactory(fixture.ConnectionString);
        _client = _factory.CreateClient();
    }

    public Task InitializeAsync() => _fixture.CleanDatabaseAsync();

    public Task DisposeAsync()
    {
        _client.Dispose();
        _factory.Dispose();
        return Task.CompletedTask;
    }

    /// <summary>
    /// テストデータのセットアップ
    /// </summary>
    private async Task SetupMasterDataAsync()
    {
        // 品目マスタを作成
        await _client.PostAsJsonAsync("/api/items", new CreateItemRequest(
            ItemCode: "PROD-001",
            ItemName: "製品A",
            Category: "Product",
            SafetyStock: 50m
        ));

        await _client.PostAsJsonAsync("/api/items", new CreateItemRequest(
            ItemCode: "MAT-001",
            ItemName: "材料A",
            Category: "Material",
            SafetyStock: 100m
        ));

        // 場所マスタを直接DBに作成
        await using var conn = _fixture.CreateConnection();
        await conn.OpenAsync();
        await using var cmd = conn.CreateCommand();
        cmd.CommandText = """
            INSERT INTO "場所マスタ" ("場所コード", "場所名", "場所区分")
            VALUES ('WH001', '資材倉庫1', '倉庫')
            ON CONFLICT ("場所コード") DO NOTHING
        """;
        await cmd.ExecuteNonQueryAsync();
    }

    /// <summary>
    /// 在庫データをセットアップ
    /// </summary>
    private async Task SetupStockDataAsync()
    {
        await SetupMasterDataAsync();

        await using var conn = _fixture.CreateConnection();
        await conn.OpenAsync();

        // 在庫情報を直接挿入
        await using var cmd = conn.CreateCommand();
        cmd.CommandText = """
            INSERT INTO "在庫情報" ("場所コード", "品目コード", "在庫数量", "合格数", "不良数", "未検査数")
            VALUES
                ('WH001', 'PROD-001', 100, 95, 3, 2),
                ('WH001', 'MAT-001', 200, 180, 10, 10)
        """;
        await cmd.ExecuteNonQueryAsync();
    }

    [Fact]
    public async Task GetInventory_EmptyDatabase_ReturnsEmptyList()
    {
        // Act
        var response = await _client.GetAsync("/api/inventory");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var stocks = await response.Content.ReadFromJsonAsync<List<StockResponse>>();
        stocks.Should().NotBeNull();
        stocks.Should().BeEmpty();
    }

    [Fact]
    public async Task GetInventory_WithStockData_ReturnsAllStocks()
    {
        // Arrange
        await SetupStockDataAsync();

        // Act
        var response = await _client.GetAsync("/api/inventory");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var stocks = await response.Content.ReadFromJsonAsync<List<StockResponse>>();
        stocks.Should().NotBeNull();
        stocks.Should().HaveCount(2);
    }

    [Fact]
    public async Task GetInventory_WithItemCodeFilter_ReturnsFilteredStocks()
    {
        // Arrange
        await SetupStockDataAsync();

        // Act
        var response = await _client.GetAsync("/api/inventory?itemCode=PROD-001");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var stocks = await response.Content.ReadFromJsonAsync<List<StockResponse>>();
        stocks.Should().NotBeNull();
        stocks.Should().HaveCount(1);
        stocks![0].ItemCode.Should().Be("PROD-001");
        stocks[0].StockQuantity.Should().Be(100m);
    }

    [Fact]
    public async Task GetInventory_WithLocationCodeFilter_ReturnsFilteredStocks()
    {
        // Arrange
        await SetupStockDataAsync();

        // Act
        var response = await _client.GetAsync("/api/inventory?locationCode=WH001");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var stocks = await response.Content.ReadFromJsonAsync<List<StockResponse>>();
        stocks.Should().NotBeNull();
        stocks.Should().HaveCount(2);
        stocks.Should().AllSatisfy(s => s.LocationCode.Should().Be("WH001"));
    }

    [Fact]
    public async Task GetInventorySummary_WithStockData_ReturnsSummaries()
    {
        // Arrange
        await SetupStockDataAsync();

        // Act
        var response = await _client.GetAsync("/api/inventory/summary");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var summaries = await response.Content.ReadFromJsonAsync<List<InventorySummaryResponse>>();
        summaries.Should().NotBeNull();
        summaries.Should().HaveCount(2);
    }

    [Fact]
    public async Task GetShortageItems_WithShortageItems_ReturnsShortageList()
    {
        // Arrange
        await SetupMasterDataAsync();

        // 安全在庫以下の在庫を作成
        await using var conn = _fixture.CreateConnection();
        await conn.OpenAsync();
        await using var cmd = conn.CreateCommand();
        cmd.CommandText = """
            INSERT INTO "在庫情報" ("場所コード", "品目コード", "在庫数量", "合格数", "不良数", "未検査数")
            VALUES
                ('WH001', 'PROD-001', 30, 30, 0, 0)  -- 安全在庫 50 より少ない
        """;
        await cmd.ExecuteNonQueryAsync();

        // Act
        var response = await _client.GetAsync("/api/inventory/shortage");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var shortageItems = await response.Content.ReadFromJsonAsync<List<InventorySummaryResponse>>();
        shortageItems.Should().NotBeNull();
        // 安全在庫未満のアイテムが含まれることを検証
        shortageItems.Should().Contain(s => s.ItemCode == "PROD-001");
    }

    [Fact]
    public async Task GetShortageItems_NoShortageItems_ReturnsEmptyList()
    {
        // Arrange
        await SetupMasterDataAsync();

        // 安全在庫以上の在庫を作成
        await using var conn = _fixture.CreateConnection();
        await conn.OpenAsync();
        await using var cmd = conn.CreateCommand();
        cmd.CommandText = """
            INSERT INTO "在庫情報" ("場所コード", "品目コード", "在庫数量", "合格数", "不良数", "未検査数")
            VALUES
                ('WH001', 'PROD-001', 100, 100, 0, 0),  -- 安全在庫 50 より多い
                ('WH001', 'MAT-001', 200, 200, 0, 0)    -- 安全在庫 100 より多い
        """;
        await cmd.ExecuteNonQueryAsync();

        // Act
        var response = await _client.GetAsync("/api/inventory/shortage");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var shortageItems = await response.Content.ReadFromJsonAsync<List<InventorySummaryResponse>>();
        shortageItems.Should().NotBeNull();
        shortageItems.Should().BeEmpty();
    }
}
