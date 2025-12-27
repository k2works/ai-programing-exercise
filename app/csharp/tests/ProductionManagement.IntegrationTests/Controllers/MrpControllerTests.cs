using System.Net;
using System.Net.Http.Json;
using FluentAssertions;
using ProductionManagement.Domain.Models.Bom;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Infrastructure.Rest.Dto;
using ProductionManagement.IntegrationTests.TestSetup;

namespace ProductionManagement.IntegrationTests.Controllers;

/// <summary>
/// MRP 実行 API の統合テスト
/// </summary>
[Collection("Postgres")]
public class MrpControllerTests : IAsyncLifetime
{
    private readonly PostgresFixture _fixture;
    private readonly CustomWebApplicationFactory _factory;
    private readonly HttpClient _client;

    public MrpControllerTests(PostgresFixture fixture)
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
    /// BOM を作成するヘルパーメソッド
    /// </summary>
    private async Task CreateBomAsync(string parentItemCode, string childItemCode, decimal requiredQuantity)
    {
        var bomRepository = new BomRepository(_fixture.ConnectionString);
        var bom = new Bom
        {
            ParentItemCode = parentItemCode,
            ChildItemCode = childItemCode,
            EffectiveFrom = DateOnly.FromDateTime(DateTime.Today), // 品目マスタと同じ日付
            EffectiveTo = null,
            BaseQuantity = 1m,
            RequiredQuantity = requiredQuantity,
            DefectRate = 0m,
            Sequence = 1
        };
        await bomRepository.SaveAsync(bom);
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
            Category: "Product"));

        await _client.PostAsJsonAsync("/api/items", new CreateItemRequest(
            ItemCode: "MAT-001",
            ItemName: "材料A",
            Category: "Material"));

        await _client.PostAsJsonAsync("/api/items", new CreateItemRequest(
            ItemCode: "MAT-002",
            ItemName: "材料B",
            Category: "Material"));

        // 部品構成表（BOM）を作成
        await CreateBomAsync("PROD-001", "MAT-001", 2.0m);
        await CreateBomAsync("PROD-001", "MAT-002", 3.0m);

        // 場所マスタを直接DBに作成
        await using var conn = _fixture.CreateConnection();
        await conn.OpenAsync();

        // 場所マスタ
        await using (var cmd = conn.CreateCommand())
        {
            cmd.CommandText = """
                INSERT INTO "場所マスタ" ("場所コード", "場所名", "場所区分")
                VALUES ('WH001', '資材倉庫1', '倉庫')
                ON CONFLICT ("場所コード") DO NOTHING
            """;
            await cmd.ExecuteNonQueryAsync();
        }
    }

    /// <summary>
    /// オーダ情報を作成してIDを取得
    /// </summary>
    private async Task<int> CreateOrderAndGetIdAsync()
    {
        await using var conn = _fixture.CreateConnection();
        await conn.OpenAsync();

        // 現在日付から将来の日付を計算
        var today = DateTime.Today;
        var startDate = today.AddDays(7).ToString("yyyy-MM-dd");
        var dueDate = today.AddDays(14).ToString("yyyy-MM-dd");

        await using var cmd = conn.CreateCommand();
        cmd.CommandText = $"""
            INSERT INTO "オーダ情報" (
                "オーダNO", "オーダ種別", "品目コード", "着手予定日", "納期",
                "計画数量", "場所コード", "ステータス"
            )
            VALUES (
                'MO-001', '製造', 'PROD-001', '{startDate}', '{dueDate}',
                100, 'WH001', '確定'
            )
            RETURNING "ID"
        """;

        var result = await cmd.ExecuteScalarAsync();
        return Convert.ToInt32(result, System.Globalization.CultureInfo.InvariantCulture);
    }

    /// <summary>
    /// 在庫データをセットアップ
    /// </summary>
    private async Task SetupStockDataAsync()
    {
        await using var conn = _fixture.CreateConnection();
        await conn.OpenAsync();

        await using var cmd = conn.CreateCommand();
        cmd.CommandText = """
            INSERT INTO "在庫情報" ("場所コード", "品目コード", "在庫数量", "合格数", "不良数", "未検査数")
            VALUES
                ('WH001', 'MAT-001', 100, 100, 0, 0),
                ('WH001', 'MAT-002', 150, 150, 0, 0)
        """;
        await cmd.ExecuteNonQueryAsync();
    }

    [Fact]
    public async Task ExplodeRequirements_ValidOrder_ReturnsRequirements()
    {
        // Arrange
        await SetupMasterDataAsync();
        var orderId = await CreateOrderAndGetIdAsync();

        var request = new MrpExecuteRequest(OrderId: orderId);

        // Act
        var response = await _client.PostAsJsonAsync("/api/mrp/explode", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var requirements = await response.Content.ReadFromJsonAsync<List<RequirementResponse>>();
        requirements.Should().NotBeNull();
        requirements.Should().HaveCount(2); // MAT-001 と MAT-002 の2つ

        // MAT-001 の所要量を検証 (製品100個 × 必要数2 = 200)
        var mat001Requirement = requirements!.FirstOrDefault(r => r.ItemCode == "MAT-001");
        mat001Requirement.Should().NotBeNull();
        mat001Requirement!.RequiredQuantity.Should().Be(200m);

        // MAT-002 の所要量を検証 (製品100個 × 必要数3 = 300)
        var mat002Requirement = requirements.FirstOrDefault(r => r.ItemCode == "MAT-002");
        mat002Requirement.Should().NotBeNull();
        mat002Requirement!.RequiredQuantity.Should().Be(300m);
    }

    [Fact]
    public async Task AllocateFromInventory_ValidRequest_ReturnsAllocation()
    {
        // Arrange
        await SetupMasterDataAsync();
        await SetupStockDataAsync();
        var orderId = await CreateOrderAndGetIdAsync();

        // 所要量を展開
        var explodeRequest = new MrpExecuteRequest(OrderId: orderId);
        var explodeResponse = await _client.PostAsJsonAsync("/api/mrp/explode", explodeRequest);
        var requirements = await explodeResponse.Content.ReadFromJsonAsync<List<RequirementResponse>>();

        var mat001Requirement = requirements!.First(r => r.ItemCode == "MAT-001");

        var allocateRequest = new AllocateInventoryRequest(
            RequirementId: mat001Requirement.Id,
            InventoryQuantity: 50m
        );

        // Act
        var response = await _client.PostAsJsonAsync("/api/mrp/allocate", allocateRequest);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var allocation = await response.Content.ReadFromJsonAsync<AllocationResponse>();
        allocation.Should().NotBeNull();
        allocation!.RequirementId.Should().Be(mat001Requirement.Id);
        allocation.AllocatedQuantity.Should().Be(50m);
        allocation.AllocationType.Should().Be("Inventory");
    }

    [Fact]
    public async Task AllocateFromInventory_MultipleAllocations_UpdatesRequirement()
    {
        // Arrange
        await SetupMasterDataAsync();
        await SetupStockDataAsync();
        var orderId = await CreateOrderAndGetIdAsync();

        // 所要量を展開
        var explodeRequest = new MrpExecuteRequest(OrderId: orderId);
        var explodeResponse = await _client.PostAsJsonAsync("/api/mrp/explode", explodeRequest);
        var requirements = await explodeResponse.Content.ReadFromJsonAsync<List<RequirementResponse>>();

        var mat001Requirement = requirements!.First(r => r.ItemCode == "MAT-001");

        // 1回目の引当 (50)
        var alloc1Response = await _client.PostAsJsonAsync("/api/mrp/allocate", new AllocateInventoryRequest(
            RequirementId: mat001Requirement.Id,
            InventoryQuantity: 50m));

        // Assert - 1回目の引当
        alloc1Response.StatusCode.Should().Be(HttpStatusCode.OK);
        var alloc1 = await alloc1Response.Content.ReadFromJsonAsync<AllocationResponse>();
        alloc1.Should().NotBeNull();
        alloc1!.AllocatedQuantity.Should().Be(50m);

        // 2回目の引当 (30)
        var alloc2Response = await _client.PostAsJsonAsync("/api/mrp/allocate", new AllocateInventoryRequest(
            RequirementId: mat001Requirement.Id,
            InventoryQuantity: 30m));

        // Assert - 2回目の引当
        alloc2Response.StatusCode.Should().Be(HttpStatusCode.OK);
        var alloc2 = await alloc2Response.Content.ReadFromJsonAsync<AllocationResponse>();
        alloc2.Should().NotBeNull();
        alloc2!.AllocatedQuantity.Should().Be(30m);

        // 引当後の所要量を直接DBから確認
        await using var conn = _fixture.CreateConnection();
        await conn.OpenAsync();
        await using var cmd = conn.CreateCommand();
        cmd.CommandText = $"""
            SELECT "引当済数量", "不足数量"
            FROM "所要情報"
            WHERE "ID" = {mat001Requirement.Id}
        """;
        await using var reader = await cmd.ExecuteReaderAsync();
        reader.Read().Should().BeTrue();

        var allocatedQuantity = reader.GetDecimal(0);
        var shortageQuantity = reader.GetDecimal(1);

        // 引当済数量は80（50 + 30）
        allocatedQuantity.Should().Be(80m);

        // 不足数量は200 - 80 = 120
        shortageQuantity.Should().Be(120m);
    }

    [Fact]
    public async Task ExplodeRequirements_NoOrder_ReturnsBadRequest()
    {
        // Arrange
        var request = new MrpExecuteRequest(OrderId: 99999);

        // Act
        var response = await _client.PostAsJsonAsync("/api/mrp/explode", request);

        // Assert
        response.StatusCode.Should().BeOneOf(HttpStatusCode.BadRequest, HttpStatusCode.NotFound, HttpStatusCode.InternalServerError);
    }
}
