using System.Net;
using System.Net.Http.Json;
using FluentAssertions;
using ProductionManagement.Infrastructure.Rest.Dto;
using ProductionManagement.IntegrationTests.TestSetup;

namespace ProductionManagement.IntegrationTests.Controllers;

[Collection("Postgres")]
public class ItemControllerTests : IAsyncLifetime
{
    private readonly PostgresFixture _fixture;
    private readonly CustomWebApplicationFactory _factory;
    private readonly HttpClient _client;

    public ItemControllerTests(PostgresFixture fixture)
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

    [Fact]
    public async Task GetAllItems_EmptyDatabase_ReturnsEmptyList()
    {
        // Act
        var response = await _client.GetAsync("/api/items");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var items = await response.Content.ReadFromJsonAsync<List<ItemResponse>>();
        items.Should().NotBeNull();
        items.Should().BeEmpty();
    }

    [Fact]
    public async Task CreateItem_ValidRequest_ReturnsCreated()
    {
        // Arrange
        var request = new CreateItemRequest(
            ItemCode: "TEST-001",
            ItemName: "テスト品目",
            Category: "Product",
            LeadTime: 5,
            SafetyStock: 10m
        );

        // Act
        var response = await _client.PostAsJsonAsync("/api/items", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        response.Headers.Location.Should().NotBeNull();
        response.Headers.Location!.ToString().Should().Contain("/api/items/TEST-001");

        var item = await response.Content.ReadFromJsonAsync<ItemResponse>();
        item.Should().NotBeNull();
        item!.ItemCode.Should().Be("TEST-001");
        item.ItemName.Should().Be("テスト品目");
        item.Category.Should().Be("Product");
        item.LeadTime.Should().Be(5);
        item.SafetyStock.Should().Be(10m);
    }

    [Fact]
    public async Task CreateItem_DuplicateItemCode_ReturnsConflict()
    {
        // Arrange
        var request = new CreateItemRequest(
            ItemCode: "DUPLICATE-001",
            ItemName: "テスト品目",
            Category: "Material"
        );

        await _client.PostAsJsonAsync("/api/items", request);

        // Act
        var response = await _client.PostAsJsonAsync("/api/items", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Conflict);
    }

    [Fact]
    public async Task GetItemByCode_ExistingItem_ReturnsItem()
    {
        // Arrange
        var request = new CreateItemRequest(
            ItemCode: "GET-001",
            ItemName: "取得テスト品目",
            Category: "SemiProduct"
        );
        await _client.PostAsJsonAsync("/api/items", request);

        // Act
        var response = await _client.GetAsync("/api/items/GET-001");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var item = await response.Content.ReadFromJsonAsync<ItemResponse>();
        item.Should().NotBeNull();
        item!.ItemCode.Should().Be("GET-001");
        item.ItemName.Should().Be("取得テスト品目");
    }

    [Fact]
    public async Task GetItemByCode_NonExistingItem_ReturnsNotFound()
    {
        // Act
        var response = await _client.GetAsync("/api/items/NOTEXIST-001");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task GetAllItems_WithCategoryFilter_ReturnsFilteredItems()
    {
        // Arrange
        await _client.PostAsJsonAsync("/api/items", new CreateItemRequest(
            ItemCode: "FG-001",
            ItemName: "完成品1",
            Category: "Product"
        ));
        await _client.PostAsJsonAsync("/api/items", new CreateItemRequest(
            ItemCode: "RM-001",
            ItemName: "原材料1",
            Category: "Material"
        ));
        await _client.PostAsJsonAsync("/api/items", new CreateItemRequest(
            ItemCode: "FG-002",
            ItemName: "完成品2",
            Category: "Product"
        ));

        // Act
        var response = await _client.GetAsync("/api/items?category=Product");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var items = await response.Content.ReadFromJsonAsync<List<ItemResponse>>();
        items.Should().HaveCount(2);
        items.Should().AllSatisfy(i => i.Category.Should().Be("Product"));
    }

    [Fact]
    public async Task UpdateItem_ExistingItem_ReturnsUpdatedItem()
    {
        // Arrange
        await _client.PostAsJsonAsync("/api/items", new CreateItemRequest(
            ItemCode: "UPDATE-001",
            ItemName: "更新前品目",
            Category: "Material",
            LeadTime: 3
        ));

        var updateRequest = new UpdateItemRequest(
            ItemName: "更新後品目",
            LeadTime: 7
        );

        // Act
        var response = await _client.PutAsJsonAsync("/api/items/UPDATE-001", updateRequest);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var item = await response.Content.ReadFromJsonAsync<ItemResponse>();
        item.Should().NotBeNull();
        item!.ItemName.Should().Be("更新後品目");
        item.LeadTime.Should().Be(7);
        item.Category.Should().Be("Material"); // 変更なし
    }

    [Fact]
    public async Task UpdateItem_NonExistingItem_ReturnsNotFound()
    {
        // Arrange
        var updateRequest = new UpdateItemRequest(ItemName: "更新テスト");

        // Act
        var response = await _client.PutAsJsonAsync("/api/items/NOTEXIST-001", updateRequest);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task DeleteItem_ExistingItem_ReturnsNoContent()
    {
        // Arrange
        await _client.PostAsJsonAsync("/api/items", new CreateItemRequest(
            ItemCode: "DELETE-001",
            ItemName: "削除テスト品目",
            Category: "Part"
        ));

        // Act
        var response = await _client.DeleteAsync("/api/items/DELETE-001");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NoContent);

        // Verify item is deleted
        var getResponse = await _client.GetAsync("/api/items/DELETE-001");
        getResponse.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task DeleteItem_NonExistingItem_ReturnsNotFound()
    {
        // Act
        var response = await _client.DeleteAsync("/api/items/NOTEXIST-001");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }
}
