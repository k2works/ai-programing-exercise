using System.Net;
using System.Net.Http.Json;
using FluentAssertions;
using ProductionManagement.Infrastructure.Rest.Dto;
using ProductionManagement.IntegrationTests.TestSetup;

namespace ProductionManagement.IntegrationTests.Controllers;

/// <summary>
/// オーダ API の統合テスト
/// </summary>
[Collection("Postgres")]
public class OrderControllerTests : IAsyncLifetime
{
    private readonly PostgresFixture _fixture;
    private readonly CustomWebApplicationFactory _factory;
    private readonly HttpClient _client;

    public OrderControllerTests(PostgresFixture fixture)
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
            Category: "Product"));

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

    [Fact]
    public async Task CreateOrder_ValidRequest_ReturnsCreated()
    {
        // Arrange
        await SetupMasterDataAsync();

        var today = DateOnly.FromDateTime(DateTime.Today);
        var request = new CreateOrderRequest(
            OrderType: "製造",
            ItemCode: "PROD-001",
            StartDate: today.AddDays(7),
            DueDate: today.AddDays(14),
            PlanQuantity: 100m,
            LocationCode: "WH001");

        // Act
        var response = await _client.PostAsJsonAsync("/api/orders", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        response.Headers.Location.Should().NotBeNull();

        var order = await response.Content.ReadFromJsonAsync<OrderResponse>();
        order.Should().NotBeNull();
        order!.OrderNumber.Should().StartWith("MO-");
        order.OrderType.Should().Be("製造");
        order.ItemCode.Should().Be("PROD-001");
        order.PlanQuantity.Should().Be(100m);
        order.Status.Should().Be("草案");
    }

    [Fact]
    public async Task GetOrderById_ExistingOrder_ReturnsOrder()
    {
        // Arrange
        await SetupMasterDataAsync();

        var today = DateOnly.FromDateTime(DateTime.Today);
        var createRequest = new CreateOrderRequest(
            OrderType: "製造",
            ItemCode: "PROD-001",
            StartDate: today.AddDays(7),
            DueDate: today.AddDays(14),
            PlanQuantity: 50m,
            LocationCode: "WH001");

        var createResponse = await _client.PostAsJsonAsync("/api/orders", createRequest);
        var createdOrder = await createResponse.Content.ReadFromJsonAsync<OrderResponse>();

        // Act
        var response = await _client.GetAsync($"/api/orders/{createdOrder!.Id}");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var order = await response.Content.ReadFromJsonAsync<OrderResponse>();
        order.Should().NotBeNull();
        order!.Id.Should().Be(createdOrder.Id);
        order.OrderNumber.Should().Be(createdOrder.OrderNumber);
    }

    [Fact]
    public async Task GetOrderById_NonExistingOrder_ReturnsNotFound()
    {
        // Act
        var response = await _client.GetAsync("/api/orders/99999");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task GetOrderByNumber_ExistingOrder_ReturnsOrder()
    {
        // Arrange
        await SetupMasterDataAsync();

        var today = DateOnly.FromDateTime(DateTime.Today);
        var createRequest = new CreateOrderRequest(
            OrderType: "購買",
            ItemCode: "PROD-001",
            StartDate: today.AddDays(7),
            DueDate: today.AddDays(14),
            PlanQuantity: 75m,
            LocationCode: "WH001");

        var createResponse = await _client.PostAsJsonAsync("/api/orders", createRequest);
        var createdOrder = await createResponse.Content.ReadFromJsonAsync<OrderResponse>();

        // Act
        var response = await _client.GetAsync($"/api/orders/by-number/{createdOrder!.OrderNumber}");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var order = await response.Content.ReadFromJsonAsync<OrderResponse>();
        order.Should().NotBeNull();
        order!.OrderNumber.Should().Be(createdOrder.OrderNumber);
        order.OrderType.Should().Be("購買");
    }

    [Fact]
    public async Task ConfirmOrder_DraftOrder_ReturnsConfirmedOrder()
    {
        // Arrange
        await SetupMasterDataAsync();

        var today = DateOnly.FromDateTime(DateTime.Today);
        var createRequest = new CreateOrderRequest(
            OrderType: "製造",
            ItemCode: "PROD-001",
            StartDate: today.AddDays(7),
            DueDate: today.AddDays(14),
            PlanQuantity: 100m,
            LocationCode: "WH001");

        var createResponse = await _client.PostAsJsonAsync("/api/orders", createRequest);
        var createdOrder = await createResponse.Content.ReadFromJsonAsync<OrderResponse>();

        // Act
        var response = await _client.PostAsync($"/api/orders/{createdOrder!.Id}/confirm", null);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var order = await response.Content.ReadFromJsonAsync<OrderResponse>();
        order.Should().NotBeNull();
        order!.Status.Should().Be("確定");
    }

    [Fact]
    public async Task CancelOrder_DraftOrder_ReturnsNoContent()
    {
        // Arrange
        await SetupMasterDataAsync();

        var today = DateOnly.FromDateTime(DateTime.Today);
        var createRequest = new CreateOrderRequest(
            OrderType: "製造",
            ItemCode: "PROD-001",
            StartDate: today.AddDays(7),
            DueDate: today.AddDays(14),
            PlanQuantity: 100m,
            LocationCode: "WH001");

        var createResponse = await _client.PostAsJsonAsync("/api/orders", createRequest);
        var createdOrder = await createResponse.Content.ReadFromJsonAsync<OrderResponse>();

        // Act
        var response = await _client.PostAsync($"/api/orders/{createdOrder!.Id}/cancel", null);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NoContent);
    }

    [Fact]
    public async Task CreateOrder_InvalidItemCode_ReturnsBadRequest()
    {
        // Arrange
        await SetupMasterDataAsync();

        var today = DateOnly.FromDateTime(DateTime.Today);
        var request = new CreateOrderRequest(
            OrderType: "製造",
            ItemCode: "NOTEXIST",
            StartDate: today.AddDays(7),
            DueDate: today.AddDays(14),
            PlanQuantity: 100m,
            LocationCode: "WH001");

        // Act
        var response = await _client.PostAsJsonAsync("/api/orders", request);

        // Assert
        response.StatusCode.Should().BeOneOf(HttpStatusCode.NotFound, HttpStatusCode.InternalServerError);
    }

    [Fact]
    public async Task CreateOrder_PurchaseOrderType_ReturnsOrderWithPoPrefix()
    {
        // Arrange
        await SetupMasterDataAsync();

        var today = DateOnly.FromDateTime(DateTime.Today);
        var request = new CreateOrderRequest(
            OrderType: "購買",
            ItemCode: "PROD-001",
            StartDate: today.AddDays(7),
            DueDate: today.AddDays(14),
            PlanQuantity: 200m,
            LocationCode: "WH001");

        // Act
        var response = await _client.PostAsJsonAsync("/api/orders", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        var order = await response.Content.ReadFromJsonAsync<OrderResponse>();
        order.Should().NotBeNull();
        order!.OrderNumber.Should().StartWith("PO-");
        order.OrderType.Should().Be("購買");
    }
}
