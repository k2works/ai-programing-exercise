using FluentAssertions;
using Grpc.Core;
using ProductionManagement.Grpc.Protos;
using ProductionManagement.IntegrationTests.TestSetup;
using DomainItemCategory = ProductionManagement.Domain.Models.Item.ItemCategory;
using ProtoItemCategory = ProductionManagement.Grpc.Protos.ItemCategory;

namespace ProductionManagement.IntegrationTests.Grpc;

/// <summary>
/// ItemGrpcService 統合テスト
/// </summary>
[Collection("Grpc")]
[Trait("Category", "Integration")]
public class ItemGrpcServiceTests : IAsyncLifetime
{
    private readonly GrpcTestFixture _fixture;
    private ItemService.ItemServiceClient _client = null!;

    public ItemGrpcServiceTests(GrpcTestFixture fixture)
    {
        _fixture = fixture;
    }

    public async Task InitializeAsync()
    {
        _client = new ItemService.ItemServiceClient(_fixture.Channel);
        await _fixture.CleanDatabaseAsync();
    }

    public Task DisposeAsync() => Task.CompletedTask;

    [Fact]
    public async Task GetItem_ExistingItem_ReturnsItem()
    {
        // Arrange
        await _fixture.InsertItemAsync("PROD-001", "テスト製品", DomainItemCategory.Product);

        // Act
        var response = await _client.GetItemAsync(new GetItemRequest { ItemCode = "PROD-001" });

        // Assert
        response.ItemCode.Should().Be("PROD-001");
        response.ItemName.Should().Be("テスト製品");
        response.Category.Should().Be(ProtoItemCategory.Product);
    }

    [Fact]
    public async Task GetItem_NonExistingItem_ThrowsNotFound()
    {
        // Act
        var act = async () => await _client.GetItemAsync(
            new GetItemRequest { ItemCode = "NOT-EXIST" });

        // Assert
        var exception = await act.Should().ThrowAsync<RpcException>();
        exception.Which.StatusCode.Should().Be(StatusCode.NotFound);
    }

    [Fact]
    public async Task CreateItem_ValidRequest_ReturnsCreatedItem()
    {
        // Arrange
        var request = new CreateItemRequest
        {
            ItemCode = "NEW-001",
            ItemName = "新規品目",
            Category = ProtoItemCategory.Part,
            LeadTime = 5,
            SafetyLeadTime = 2
        };

        // Act
        var response = await _client.CreateItemAsync(request);

        // Assert
        response.ItemCode.Should().Be("NEW-001");
        response.ItemName.Should().Be("新規品目");
        response.Category.Should().Be(ProtoItemCategory.Part);
        response.LeadTime.Should().Be(5);
        response.SafetyLeadTime.Should().Be(2);
    }

    [Fact]
    public async Task CreateItem_DuplicateCode_ThrowsAlreadyExists()
    {
        // Arrange
        await _fixture.InsertItemAsync("DUP-001", "既存品目", DomainItemCategory.Product);

        var request = new CreateItemRequest
        {
            ItemCode = "DUP-001",
            ItemName = "重複品目",
            Category = ProtoItemCategory.Product
        };

        // Act
        var act = async () => await _client.CreateItemAsync(request);

        // Assert
        var exception = await act.Should().ThrowAsync<RpcException>();
        exception.Which.StatusCode.Should().Be(StatusCode.AlreadyExists);
    }

    [Fact]
    public async Task UpdateItem_ExistingItem_ReturnsUpdatedItem()
    {
        // Arrange
        await _fixture.InsertItemAsync("UPD-001", "更新前品目", DomainItemCategory.Material);

        var request = new UpdateItemRequest
        {
            ItemCode = "UPD-001",
            ItemName = "更新後品目",
            LeadTime = 10
        };

        // Act
        var response = await _client.UpdateItemAsync(request);

        // Assert
        response.ItemCode.Should().Be("UPD-001");
        response.ItemName.Should().Be("更新後品目");
        response.LeadTime.Should().Be(10);
    }

    [Fact]
    public async Task UpdateItem_NonExistingItem_ThrowsNotFound()
    {
        // Arrange
        var request = new UpdateItemRequest
        {
            ItemCode = "NOT-EXIST",
            ItemName = "存在しない品目"
        };

        // Act
        var act = async () => await _client.UpdateItemAsync(request);

        // Assert
        var exception = await act.Should().ThrowAsync<RpcException>();
        exception.Which.StatusCode.Should().Be(StatusCode.NotFound);
    }

    [Fact]
    public async Task DeleteItem_ExistingItem_ReturnsEmpty()
    {
        // Arrange
        await _fixture.InsertItemAsync("DEL-001", "削除対象品目", DomainItemCategory.Product);

        // Act
        var response = await _client.DeleteItemAsync(new DeleteItemRequest { ItemCode = "DEL-001" });

        // Assert
        response.Should().NotBeNull();

        // 削除後に取得しようとするとエラー
        var act = async () => await _client.GetItemAsync(new GetItemRequest { ItemCode = "DEL-001" });
        var exception = await act.Should().ThrowAsync<RpcException>();
        exception.Which.StatusCode.Should().Be(StatusCode.NotFound);
    }

    [Fact]
    public async Task DeleteItem_NonExistingItem_ThrowsNotFound()
    {
        // Act
        var act = async () => await _client.DeleteItemAsync(
            new DeleteItemRequest { ItemCode = "NOT-EXIST" });

        // Assert
        var exception = await act.Should().ThrowAsync<RpcException>();
        exception.Which.StatusCode.Should().Be(StatusCode.NotFound);
    }

    [Fact]
    public async Task StreamItems_MultipleItems_StreamsAll()
    {
        // Arrange
        for (int i = 1; i <= 5; i++)
        {
            await _fixture.InsertItemAsync($"ITEM-{i:000}", $"品目{i}", DomainItemCategory.Product);
        }

        // Act
        var items = new List<ItemMessage>();
        using var call = _client.StreamItems(new GetItemsRequest());
        await foreach (var item in call.ResponseStream.ReadAllAsync())
        {
            items.Add(item);
        }

        // Assert
        items.Should().HaveCount(5);
        items.Should().AllSatisfy(item => item.Category.Should().Be(ProtoItemCategory.Product));
    }

    [Fact]
    public async Task StreamItems_FilterByCategory_ReturnsFilteredItems()
    {
        // Arrange
        await _fixture.InsertItemAsync("PROD-001", "製品1", DomainItemCategory.Product);
        await _fixture.InsertItemAsync("PROD-002", "製品2", DomainItemCategory.Product);
        await _fixture.InsertItemAsync("PART-001", "部品1", DomainItemCategory.Part);

        // Act
        var items = new List<ItemMessage>();
        using var call = _client.StreamItems(new GetItemsRequest { Category = ProtoItemCategory.Product });
        await foreach (var item in call.ResponseStream.ReadAllAsync())
        {
            items.Add(item);
        }

        // Assert
        items.Should().HaveCount(2);
        items.Should().AllSatisfy(item => item.Category.Should().Be(ProtoItemCategory.Product));
    }

    [Fact]
    public async Task BatchCreateItems_MultipleItems_ReturnsSuccessCount()
    {
        // Arrange
        var requests = Enumerable.Range(1, 3)
            .Select(i => new CreateItemRequest
            {
                ItemCode = $"BATCH-{i:000}",
                ItemName = $"バッチ品目{i}",
                Category = ProtoItemCategory.Product
            });

        // Act
        using var call = _client.BatchCreateItems();
        foreach (var request in requests)
        {
            await call.RequestStream.WriteAsync(request);
        }
        await call.RequestStream.CompleteAsync();

        var response = await call.ResponseAsync;

        // Assert
        response.SuccessCount.Should().Be(3);
        response.FailedCount.Should().Be(0);
        response.FailedCodes.Should().BeEmpty();
    }

    [Fact]
    public async Task BatchCreateItems_WithDuplicates_ReturnsPartialSuccess()
    {
        // Arrange
        await _fixture.InsertItemAsync("EXIST-001", "既存品目", DomainItemCategory.Product);

        var requests = new[]
        {
            new CreateItemRequest { ItemCode = "NEW-001", ItemName = "新規品目1", Category = ProtoItemCategory.Product },
            new CreateItemRequest { ItemCode = "EXIST-001", ItemName = "重複品目", Category = ProtoItemCategory.Product },
            new CreateItemRequest { ItemCode = "NEW-002", ItemName = "新規品目2", Category = ProtoItemCategory.Product }
        };

        // Act
        using var call = _client.BatchCreateItems();
        foreach (var request in requests)
        {
            await call.RequestStream.WriteAsync(request);
        }
        await call.RequestStream.CompleteAsync();

        var response = await call.ResponseAsync;

        // Assert
        response.SuccessCount.Should().Be(2);
        response.FailedCount.Should().Be(1);
        response.FailedCodes.Should().Contain("EXIST-001");
    }

    [Fact]
    public async Task SearchItems_WithKeyword_ReturnsMatchingItems()
    {
        // Arrange
        await _fixture.InsertItemAsync("SRCH-001", "検索対象品目A", DomainItemCategory.Product);
        await _fixture.InsertItemAsync("SRCH-002", "検索対象品目B", DomainItemCategory.Product);
        await _fixture.InsertItemAsync("OTHER-001", "その他品目", DomainItemCategory.Part);

        // Act
        var response = await _client.SearchItemsAsync(new SearchItemsRequest { Keyword = "検索対象" });

        // Assert
        response.Items.Should().HaveCount(2);
        response.Items.Should().AllSatisfy(item => item.ItemName.Should().Contain("検索対象"));
    }
}
