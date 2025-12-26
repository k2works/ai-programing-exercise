using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// 品目リポジトリテスト
/// </summary>
[Collection("Database")]
public class ItemRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly IItemRepository _itemRepository;

    public ItemRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _itemRepository.DeleteAllAsync().Wait();
    }

    public class Registration : ItemRepositoryTests
    {
        public Registration(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 品目を登録できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "ITEM001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品A",
                ItemCategory = ItemCategory.Product
            };

            // Act
            await _itemRepository.SaveAsync(item);

            // Assert
            item.Id.Should().NotBe(0);

            var result = await _itemRepository.FindByItemCodeAsync("ITEM001");
            result.Should().NotBeNull();
            result!.ItemName.Should().Be("製品A");
            result.ItemCategory.Should().Be(ItemCategory.Product);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 同じ品目コードでも適用開始日が異なれば登録できる()
        {
            // Arrange
            var item1 = new Item
            {
                ItemCode = "ITEM001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品A（旧）",
                ItemCategory = ItemCategory.Product
            };
            await _itemRepository.SaveAsync(item1);

            var item2 = new Item
            {
                ItemCode = "ITEM001",
                EffectiveFrom = new DateOnly(2025, 4, 1),
                ItemName = "製品A（新）",
                ItemCategory = ItemCategory.Product
            };

            // Act
            await _itemRepository.SaveAsync(item2);

            // Assert
            item2.Id.Should().NotBe(0);
            item2.Id.Should().NotBe(item1.Id);
        }
    }

    public class ProductionAttributes : ItemRepositoryTests
    {
        public ProductionAttributes(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task リードタイムと安全在庫を設定できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "ITEM002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品B",
                ItemCategory = ItemCategory.Product,
                LeadTime = 5,
                SafetyLeadTime = 2,
                SafetyStock = 100m
            };

            // Act
            await _itemRepository.SaveAsync(item);

            // Assert
            var result = await _itemRepository.FindByItemCodeAsync("ITEM002");
            result.Should().NotBeNull();
            result!.LeadTime.Should().Be(5);
            result.SafetyLeadTime.Should().Be(2);
            result.SafetyStock.Should().Be(100m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task ロットサイズを設定できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "ITEM003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品C",
                ItemCategory = ItemCategory.Product,
                MinLotSize = 10m,
                LotIncrement = 5m,
                MaxLotSize = 1000m
            };

            // Act
            await _itemRepository.SaveAsync(item);

            // Assert
            var result = await _itemRepository.FindByItemCodeAsync("ITEM003");
            result.Should().NotBeNull();
            result!.MinLotSize.Should().Be(10m);
            result.LotIncrement.Should().Be(5m);
            result.MaxLotSize.Should().Be(1000m);
        }
    }

    public class ItemCategories : ItemRepositoryTests
    {
        public ItemCategories(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 全ての品目区分を登録できる()
        {
            var categories = Enum.GetValues<ItemCategory>();
            var index = 0;

            foreach (var category in categories)
            {
                var item = new Item
                {
                    ItemCode = $"CAT-{index:D3}",
                    EffectiveFrom = new DateOnly(2025, 1, 1),
                    ItemName = $"品目{category.GetDisplayName()}",
                    ItemCategory = category
                };

                await _itemRepository.SaveAsync(item);

                var result = await _itemRepository.FindByItemCodeAsync(item.ItemCode);
                result.Should().NotBeNull();
                result!.ItemCategory.Should().Be(category);

                index++;
            }
        }
    }
}
