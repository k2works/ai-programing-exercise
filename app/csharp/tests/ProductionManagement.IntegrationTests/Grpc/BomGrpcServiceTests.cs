using FluentAssertions;
using Grpc.Core;
using ProductionManagement.Grpc.Protos;
using ProductionManagement.IntegrationTests.TestSetup;
using DomainItemCategory = ProductionManagement.Domain.Models.Item.ItemCategory;

namespace ProductionManagement.IntegrationTests.Grpc;

/// <summary>
/// BomGrpcService 統合テスト
/// </summary>
[Collection("Grpc")]
[Trait("Category", "Integration")]
public class BomGrpcServiceTests : IAsyncLifetime
{
    private readonly GrpcTestFixture _fixture;
    private BomService.BomServiceClient _client = null!;

    public BomGrpcServiceTests(GrpcTestFixture fixture)
    {
        _fixture = fixture;
    }

    public async Task InitializeAsync()
    {
        _client = new BomService.BomServiceClient(_fixture.Channel);
        await _fixture.CleanDatabaseAsync();
    }

    public Task DisposeAsync() => Task.CompletedTask;

    [Fact]
    public async Task ExplodeBom_SingleLevel_ReturnsNodes()
    {
        // Arrange
        // 製品 -> 部品A, 部品B
        await _fixture.InsertItemAsync("PROD-001", "製品1", DomainItemCategory.Product);
        await _fixture.InsertItemAsync("PART-001", "部品A", DomainItemCategory.Part);
        await _fixture.InsertItemAsync("PART-002", "部品B", DomainItemCategory.Part);
        await _fixture.InsertBomAsync("PROD-001", "PART-001", 2.0m);
        await _fixture.InsertBomAsync("PROD-001", "PART-002", 3.0m);

        // Act
        var nodes = new List<BomNodeMessage>();
        using var call = _client.ExplodeBom(new ExplodeBomRequest { ItemCode = "PROD-001" });
        await foreach (var node in call.ResponseStream.ReadAllAsync())
        {
            nodes.Add(node);
        }

        // Assert
        nodes.Should().HaveCount(3); // 製品 + 部品2つ
        nodes[0].ItemCode.Should().Be("PROD-001");
        nodes[0].Level.Should().Be(0);
    }

    [Fact]
    public async Task ExplodeBom_MultiLevel_ReturnsAllNodes()
    {
        // Arrange
        // 製品 -> 半製品 -> 部品
        await _fixture.InsertItemAsync("PROD-001", "製品1", DomainItemCategory.Product);
        await _fixture.InsertItemAsync("SEMI-001", "半製品1", DomainItemCategory.SemiProduct);
        await _fixture.InsertItemAsync("PART-001", "部品1", DomainItemCategory.Part);
        await _fixture.InsertBomAsync("PROD-001", "SEMI-001", 1.0m);
        await _fixture.InsertBomAsync("SEMI-001", "PART-001", 5.0m);

        // Act
        var nodes = new List<BomNodeMessage>();
        using var call = _client.ExplodeBom(new ExplodeBomRequest { ItemCode = "PROD-001" });
        await foreach (var node in call.ResponseStream.ReadAllAsync())
        {
            nodes.Add(node);
        }

        // Assert
        nodes.Should().HaveCount(3);
        nodes.Should().Contain(n => n.ItemCode == "PROD-001" && n.Level == 0);
        nodes.Should().Contain(n => n.ItemCode == "SEMI-001" && n.Level == 1);
        nodes.Should().Contain(n => n.ItemCode == "PART-001" && n.Level == 2);
    }

    [Fact]
    public async Task ExplodeBom_NonExistingItem_ThrowsNotFound()
    {
        // Act
        var act = async () =>
        {
            using var call = _client.ExplodeBom(new ExplodeBomRequest { ItemCode = "NOT-EXIST" });
            await foreach (var _ in call.ResponseStream.ReadAllAsync())
            {
                // consume stream
            }
        };

        // Assert
        var exception = await act.Should().ThrowAsync<RpcException>();
        exception.Which.StatusCode.Should().Be(StatusCode.NotFound);
    }

    [Fact]
    public async Task ExplodeBom_LeafItem_ReturnsOnlyRootNode()
    {
        // Arrange - 子を持たない品目
        await _fixture.InsertItemAsync("LEAF-001", "末端品目", DomainItemCategory.Material);

        // Act
        var nodes = new List<BomNodeMessage>();
        using var call = _client.ExplodeBom(new ExplodeBomRequest { ItemCode = "LEAF-001" });
        await foreach (var node in call.ResponseStream.ReadAllAsync())
        {
            nodes.Add(node);
        }

        // Assert
        nodes.Should().ContainSingle();
        nodes[0].ItemCode.Should().Be("LEAF-001");
        nodes[0].Level.Should().Be(0);
    }

    [Fact]
    public async Task WhereUsed_ExistingItem_ReturnsParents()
    {
        // Arrange
        // 製品A, 製品B -> 共通部品
        await _fixture.InsertItemAsync("PROD-001", "製品A", DomainItemCategory.Product);
        await _fixture.InsertItemAsync("PROD-002", "製品B", DomainItemCategory.Product);
        await _fixture.InsertItemAsync("PART-001", "共通部品", DomainItemCategory.Part);
        await _fixture.InsertBomAsync("PROD-001", "PART-001", 1.0m);
        await _fixture.InsertBomAsync("PROD-002", "PART-001", 2.0m);

        // Act
        var response = await _client.WhereUsedAsync(new WhereUsedRequest { ItemCode = "PART-001" });

        // Assert
        response.Results.Should().HaveCount(2);
        response.Results.Should().Contain(r => r.ParentItemCode == "PROD-001");
        response.Results.Should().Contain(r => r.ParentItemCode == "PROD-002");
    }

    [Fact]
    public async Task WhereUsed_TopLevelItem_ReturnsEmpty()
    {
        // Arrange - 親を持たない製品
        await _fixture.InsertItemAsync("TOP-001", "最上位製品", DomainItemCategory.Product);

        // Act
        var response = await _client.WhereUsedAsync(new WhereUsedRequest { ItemCode = "TOP-001" });

        // Assert
        response.Results.Should().BeEmpty();
    }

    [Fact]
    public async Task WhereUsed_ReturnsQuantityInfo()
    {
        // Arrange
        await _fixture.InsertItemAsync("PROD-001", "製品1", DomainItemCategory.Product);
        await _fixture.InsertItemAsync("PART-001", "部品1", DomainItemCategory.Part);
        await _fixture.InsertBomAsync("PROD-001", "PART-001", 5.5m);

        // Act
        var response = await _client.WhereUsedAsync(new WhereUsedRequest { ItemCode = "PART-001" });

        // Assert
        response.Results.Should().ContainSingle();
        var result = response.Results.First();
        result.ParentItemCode.Should().Be("PROD-001");
        result.ItemName.Should().Be("製品1");
        result.RequiredQuantity.Value.Should().Be("5.5");
    }
}
