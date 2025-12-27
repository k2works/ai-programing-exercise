using System.Net;
using System.Net.Http.Json;
using FluentAssertions;
using ProductionManagement.Infrastructure.Rest.Dto;
using ProductionManagement.IntegrationTests.TestSetup;

namespace ProductionManagement.IntegrationTests.Controllers;

/// <summary>
/// 発注 API の統合テスト
/// </summary>
[Collection("Postgres")]
public class PurchaseOrderControllerTests : IAsyncLifetime
{
    private readonly PostgresFixture _fixture;
    private readonly CustomWebApplicationFactory _factory;
    private readonly HttpClient _client;

    public PurchaseOrderControllerTests(PostgresFixture fixture)
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
    /// テストデータのセットアップ - 直接DBに挿入
    /// </summary>
    private async Task SetupMasterDataAsync()
    {
        // 品目マスタを作成
        await _client.PostAsJsonAsync("/api/items", new CreateItemRequest(
            ItemCode: "MAT-001",
            ItemName: "テスト材料",
            Category: "Material"));

        // 取引先マスタと単価マスタを直接DBに作成
        await using var conn = _fixture.CreateConnection();
        await conn.OpenAsync();

        // 取引先マスタ（複合主キー: 取引先コード + 適用開始日）
        await using (var cmd = conn.CreateCommand())
        {
            cmd.CommandText = """
                INSERT INTO "取引先マスタ" ("取引先コード", "適用開始日", "取引先名", "取引先区分")
                VALUES ('SUP-001', '2025-01-01', 'テスト取引先', '仕入先')
                ON CONFLICT ("取引先コード", "適用開始日") DO NOTHING
            """;
            await cmd.ExecuteNonQueryAsync();
        }

        // 単価マスタ（ID は IDENTITY、ユニーク制約は複合キー）
        await using (var cmd = conn.CreateCommand())
        {
            cmd.CommandText = """
                INSERT INTO "単価マスタ" ("品目コード", "取引先コード", "ロット単位数", "使用開始日", "単価")
                VALUES ('MAT-001', 'SUP-001', 1, '2025-01-01', 100)
                ON CONFLICT ON CONSTRAINT "uq_単価マスタ_品目取引先開始日" DO NOTHING
            """;
            await cmd.ExecuteNonQueryAsync();
        }
    }

    [Fact]
    public async Task GetAllOrders_EmptyDatabase_ReturnsEmptyList()
    {
        // Act
        var response = await _client.GetAsync("/api/purchase-orders");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var orders = await response.Content.ReadFromJsonAsync<List<PurchaseOrderResponse>>();
        orders.Should().NotBeNull();
        orders.Should().BeEmpty();
    }

    [Fact]
    public async Task CreateOrder_ValidRequest_ReturnsCreated()
    {
        // Arrange
        await SetupMasterDataAsync();

        var request = new CreatePurchaseOrderRequest(
            SupplierCode: "SUP-001",
            OrderDate: new DateOnly(2025, 1, 15),
            OrdererCode: null,
            DepartmentCode: null,
            TaxRate: 10m,
            Remarks: "テスト発注",
            Details:
            [
                new CreatePurchaseOrderDetailRequest(
                    ItemCode: "MAT-001",
                    OrderQuantity: 100m,
                    ExpectedReceivingDate: new DateOnly(2025, 1, 25),
                    OrderNumber: null,
                    DeliveryLocationCode: null
                )
            ]
        );

        // Act
        var response = await _client.PostAsJsonAsync("/api/purchase-orders", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        response.Headers.Location.Should().NotBeNull();

        var order = await response.Content.ReadFromJsonAsync<PurchaseOrderResponse>();
        order.Should().NotBeNull();
        order!.SupplierCode.Should().Be("SUP-001");
        order.Status.Should().Be("作成中");
        order.Details.Should().HaveCount(1);
        order.Details[0].ItemCode.Should().Be("MAT-001");
        order.Details[0].OrderQuantity.Should().Be(100m);
    }

    [Fact]
    public async Task GetOrder_ExistingOrder_ReturnsOrder()
    {
        // Arrange
        await SetupMasterDataAsync();

        var createRequest = new CreatePurchaseOrderRequest(
            SupplierCode: "SUP-001",
            OrderDate: new DateOnly(2025, 1, 15),
            OrdererCode: null,
            DepartmentCode: null,
            TaxRate: 10m,
            Remarks: null,
            Details:
            [
                new CreatePurchaseOrderDetailRequest(
                    ItemCode: "MAT-001",
                    OrderQuantity: 50m,
                    ExpectedReceivingDate: new DateOnly(2025, 1, 20),
                    OrderNumber: null,
                    DeliveryLocationCode: null
                )
            ]
        );

        var createResponse = await _client.PostAsJsonAsync("/api/purchase-orders", createRequest);
        var createdOrder = await createResponse.Content.ReadFromJsonAsync<PurchaseOrderResponse>();

        // Act
        var response = await _client.GetAsync($"/api/purchase-orders/{createdOrder!.PurchaseOrderNumber}");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var order = await response.Content.ReadFromJsonAsync<PurchaseOrderResponse>();
        order.Should().NotBeNull();
        order!.PurchaseOrderNumber.Should().Be(createdOrder.PurchaseOrderNumber);
        order.Details.Should().HaveCount(1);
    }

    [Fact]
    public async Task GetOrder_NonExistingOrder_ReturnsNotFound()
    {
        // Act
        var response = await _client.GetAsync("/api/purchase-orders/NOTEXIST-001");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task ConfirmOrder_ValidOrder_ReturnsOk()
    {
        // Arrange
        await SetupMasterDataAsync();

        var createRequest = new CreatePurchaseOrderRequest(
            SupplierCode: "SUP-001",
            OrderDate: new DateOnly(2025, 1, 15),
            OrdererCode: null,
            DepartmentCode: null,
            TaxRate: 10m,
            Remarks: null,
            Details:
            [
                new CreatePurchaseOrderDetailRequest(
                    ItemCode: "MAT-001",
                    OrderQuantity: 100m,
                    ExpectedReceivingDate: new DateOnly(2025, 1, 25),
                    OrderNumber: null,
                    DeliveryLocationCode: null
                )
            ]
        );

        var createResponse = await _client.PostAsJsonAsync("/api/purchase-orders", createRequest);
        var createdOrder = await createResponse.Content.ReadFromJsonAsync<PurchaseOrderResponse>();

        // Act
        var response = await _client.PostAsync(
            $"/api/purchase-orders/{createdOrder!.PurchaseOrderNumber}/confirm",
            null
        );

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var order = await response.Content.ReadFromJsonAsync<PurchaseOrderResponse>();
        order.Should().NotBeNull();
        order!.Status.Should().Be("発注済");
    }

    [Fact]
    public async Task CancelOrder_ValidOrder_ReturnsNoContent()
    {
        // Arrange
        await SetupMasterDataAsync();

        var createRequest = new CreatePurchaseOrderRequest(
            SupplierCode: "SUP-001",
            OrderDate: new DateOnly(2025, 1, 15),
            OrdererCode: null,
            DepartmentCode: null,
            TaxRate: 10m,
            Remarks: null,
            Details:
            [
                new CreatePurchaseOrderDetailRequest(
                    ItemCode: "MAT-001",
                    OrderQuantity: 100m,
                    ExpectedReceivingDate: new DateOnly(2025, 1, 25),
                    OrderNumber: null,
                    DeliveryLocationCode: null
                )
            ]
        );

        var createResponse = await _client.PostAsJsonAsync("/api/purchase-orders", createRequest);
        var createdOrder = await createResponse.Content.ReadFromJsonAsync<PurchaseOrderResponse>();

        // Act
        var response = await _client.PostAsync(
            $"/api/purchase-orders/{createdOrder!.PurchaseOrderNumber}/cancel",
            null
        );

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NoContent);
    }

    [Fact]
    public async Task GetAllOrders_AfterCreatingOrders_ReturnsAllOrders()
    {
        // Arrange
        await SetupMasterDataAsync();

        // 2件の発注を作成
        for (var i = 1; i <= 2; i++)
        {
            var request = new CreatePurchaseOrderRequest(
                SupplierCode: "SUP-001",
                OrderDate: DateOnly.FromDateTime(DateTime.Today).AddDays(i),
                OrdererCode: null,
                DepartmentCode: null,
                TaxRate: 10m,
                Remarks: null,
                Details:
                [
                    new CreatePurchaseOrderDetailRequest(
                        ItemCode: "MAT-001",
                        OrderQuantity: 50m * i,
                        ExpectedReceivingDate: DateOnly.FromDateTime(DateTime.Today).AddDays(10 + i),
                        OrderNumber: null,
                        DeliveryLocationCode: null
                    )
                ]
            );
            await _client.PostAsJsonAsync("/api/purchase-orders", request);
        }

        // Act
        var response = await _client.GetAsync("/api/purchase-orders");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var orders = await response.Content.ReadFromJsonAsync<List<PurchaseOrderResponse>>();
        orders.Should().HaveCount(2);
    }
}
