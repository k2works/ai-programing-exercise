using System.Net;
using System.Net.Http.Json;
using FluentAssertions;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Models.Bom;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Infrastructure.Rest.Dto;
using ProductionManagement.IntegrationTests.TestSetup;

namespace ProductionManagement.IntegrationTests.Controllers;

[Collection("Postgres")]
public class BomControllerTests : IAsyncLifetime
{
    private readonly PostgresFixture _fixture;
    private readonly CustomWebApplicationFactory _factory;
    private readonly HttpClient _client;

    public BomControllerTests(PostgresFixture fixture)
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

    private async Task CreateTestItemAsync(string itemCode, string itemName, string category)
    {
        var request = new CreateItemRequest(
            ItemCode: itemCode,
            ItemName: itemName,
            Category: category
        );
        await _client.PostAsJsonAsync("/api/items", request);
    }

    private async Task CreateBomAsync(string parentItemCode, string childItemCode, decimal requiredQuantity)
    {
        var bomRepository = new BomRepository(_fixture.ConnectionString);
        var bom = new Bom
        {
            ParentItemCode = parentItemCode,
            ChildItemCode = childItemCode,
            EffectiveFrom = DateOnly.FromDateTime(DateTime.Today),
            EffectiveTo = null,
            BaseQuantity = 1m,
            RequiredQuantity = requiredQuantity,
            DefectRate = 0m,
            Sequence = 1
        };
        await bomRepository.SaveAsync(bom);
    }

    [Fact]
    public async Task ExplodeBom_SingleLevel_ReturnsBomNode()
    {
        // Arrange
        await CreateTestItemAsync("PROD-001", "完成品A", "Product");
        await CreateTestItemAsync("PART-001", "部品A", "Part");
        await CreateTestItemAsync("PART-002", "部品B", "Part");

        await CreateBomAsync("PROD-001", "PART-001", 2m);
        await CreateBomAsync("PROD-001", "PART-002", 3m);

        // Act
        var response = await _client.GetAsync("/api/bom/PROD-001/explode");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var bomNode = await response.Content.ReadFromJsonAsync<BomNode>();

        bomNode.Should().NotBeNull();
        bomNode!.ItemCode.Should().Be("PROD-001");
        bomNode.ItemName.Should().Be("完成品A");
        bomNode.Level.Should().Be(0);
        bomNode.Children.Should().HaveCount(2);
        bomNode.Children.Should().Contain(c => c.ItemCode == "PART-001" && c.RequiredQuantity == 2m);
        bomNode.Children.Should().Contain(c => c.ItemCode == "PART-002" && c.RequiredQuantity == 3m);
    }

    [Fact]
    public async Task ExplodeBom_MultiLevel_ReturnsNestedBomNode()
    {
        // Arrange
        // 完成品 -> 中間品 -> 原材料
        await CreateTestItemAsync("PROD-ML", "完成品", "Product");
        await CreateTestItemAsync("SEMI-ML", "中間品", "SemiProduct");
        await CreateTestItemAsync("RAW-ML", "原材料", "RawMaterial");

        await CreateBomAsync("PROD-ML", "SEMI-ML", 1m);
        await CreateBomAsync("SEMI-ML", "RAW-ML", 5m);

        // Act
        var response = await _client.GetAsync("/api/bom/PROD-ML/explode");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var bomNode = await response.Content.ReadFromJsonAsync<BomNode>();

        bomNode.Should().NotBeNull();
        bomNode!.ItemCode.Should().Be("PROD-ML");
        bomNode.Level.Should().Be(0);
        bomNode.Children.Should().HaveCount(1);

        var semiNode = bomNode.Children[0];
        semiNode.ItemCode.Should().Be("SEMI-ML");
        semiNode.Level.Should().Be(1);
        semiNode.Children.Should().HaveCount(1);

        var rawNode = semiNode.Children[0];
        rawNode.ItemCode.Should().Be("RAW-ML");
        rawNode.Level.Should().Be(2);
        rawNode.Children.Should().BeEmpty();
    }

    [Fact]
    public async Task ExplodeBom_NoChildren_ReturnsEmptyChildren()
    {
        // Arrange
        await CreateTestItemAsync("LEAF-001", "末端品目", "Part");

        // Act
        var response = await _client.GetAsync("/api/bom/LEAF-001/explode");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var bomNode = await response.Content.ReadFromJsonAsync<BomNode>();

        bomNode.Should().NotBeNull();
        bomNode!.ItemCode.Should().Be("LEAF-001");
        bomNode.Children.Should().BeEmpty();
    }

    [Fact]
    public async Task ExplodeBom_NonExistingItem_ReturnsNotFound()
    {
        // Act
        var response = await _client.GetAsync("/api/bom/NOTEXIST-001/explode");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task WhereUsed_SingleParent_ReturnsParentList()
    {
        // Arrange
        await CreateTestItemAsync("PARENT-WU", "親品目", "Product");
        await CreateTestItemAsync("CHILD-WU", "子品目", "Part");

        await CreateBomAsync("PARENT-WU", "CHILD-WU", 4m);

        // Act
        var response = await _client.GetAsync("/api/bom/CHILD-WU/where-used");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var results = await response.Content.ReadFromJsonAsync<List<WhereUsedResult>>();

        results.Should().NotBeNull();
        results.Should().HaveCount(1);
        results![0].ParentItemCode.Should().Be("PARENT-WU");
        results[0].ItemName.Should().Be("親品目");
        results[0].RequiredQuantity.Should().Be(4m);
    }

    [Fact]
    public async Task WhereUsed_MultipleParents_ReturnsAllParents()
    {
        // Arrange
        await CreateTestItemAsync("PARENT-A", "親品目A", "Product");
        await CreateTestItemAsync("PARENT-B", "親品目B", "Product");
        await CreateTestItemAsync("COMMON-PART", "共通部品", "Part");

        await CreateBomAsync("PARENT-A", "COMMON-PART", 2m);
        await CreateBomAsync("PARENT-B", "COMMON-PART", 3m);

        // Act
        var response = await _client.GetAsync("/api/bom/COMMON-PART/where-used");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var results = await response.Content.ReadFromJsonAsync<List<WhereUsedResult>>();

        results.Should().NotBeNull();
        results.Should().HaveCount(2);
        results.Should().Contain(r => r.ParentItemCode == "PARENT-A" && r.RequiredQuantity == 2m);
        results.Should().Contain(r => r.ParentItemCode == "PARENT-B" && r.RequiredQuantity == 3m);
    }

    [Fact]
    public async Task WhereUsed_NoParent_ReturnsEmptyList()
    {
        // Arrange
        await CreateTestItemAsync("TOP-ITEM", "最上位品目", "Product");

        // Act
        var response = await _client.GetAsync("/api/bom/TOP-ITEM/where-used");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var results = await response.Content.ReadFromJsonAsync<List<WhereUsedResult>>();

        results.Should().NotBeNull();
        results.Should().BeEmpty();
    }

    [Fact]
    public async Task WhereUsed_NonExistingItem_ReturnsEmptyList()
    {
        // Act - 存在しない品目でも空リストを返す（エラーではない）
        var response = await _client.GetAsync("/api/bom/NOTEXIST-WU/where-used");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var results = await response.Content.ReadFromJsonAsync<List<WhereUsedResult>>();
        results.Should().BeEmpty();
    }
}
