using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Bom;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// BOM（部品構成表）リポジトリテスト
/// </summary>
[Collection("Database")]
public class BomRepositoryTests
{
    private readonly IBomRepository _bomRepository;
    private readonly IItemRepository _itemRepository;

    public BomRepositoryTests(PostgresFixture fixture)
    {
        _bomRepository = new BomRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);

        _bomRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    public class Registration : BomRepositoryTests
    {
        public Registration(PostgresFixture fixture)
            : base(fixture)
        {
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 親品目と子品目の関係を登録できる()
        {
            // Arrange: 親品目と子品目を作成
            var parent = CreateItem("PRODUCT-X", "製品X", ItemCategory.Product);
            await _itemRepository.SaveAsync(parent);

            var child = CreateItem("PART-A", "部品A", ItemCategory.Part);
            await _itemRepository.SaveAsync(child);

            // Act: BOMを登録
            var bom = new Bom
            {
                ParentItemCode = "PRODUCT-X",
                ChildItemCode = "PART-A",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                BaseQuantity = 1m,
                RequiredQuantity = 2m
            };
            await _bomRepository.SaveAsync(bom);

            // Assert
            var children = await _bomRepository.FindByParentItemCodeAsync("PRODUCT-X");
            children.Should().HaveCount(1);
            children[0].ChildItemCode.Should().Be("PART-A");
            children[0].RequiredQuantity.Should().Be(2m);
        }
    }

    public class Explosion : BomRepositoryTests
    {
        public Explosion(PostgresFixture fixture)
            : base(fixture)
        {
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 単一階層の部品展開ができる()
        {
            // Arrange
            var product = CreateItem("PRODUCT-X", "製品X", ItemCategory.Product);
            await _itemRepository.SaveAsync(product);

            var partA = CreateItem("PART-A", "部品A", ItemCategory.Part);
            await _itemRepository.SaveAsync(partA);

            var partB = CreateItem("PART-B", "部品B", ItemCategory.Part);
            await _itemRepository.SaveAsync(partB);

            // BOMを登録
            await _bomRepository.SaveAsync(CreateBom("PRODUCT-X", "PART-A", 2));
            await _bomRepository.SaveAsync(CreateBom("PRODUCT-X", "PART-B", 3));

            // Act
            var children = await _bomRepository.FindByParentItemCodeAsync("PRODUCT-X");

            // Assert
            children.Should().HaveCount(2);
            children.Select(b => b.ChildItemCode).Should().Contain(new[] { "PART-A", "PART-B" });
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 不良率を考慮した必要数量を計算できる()
        {
            // Arrange
            var product = CreateItem("PRODUCT-Y", "製品Y", ItemCategory.Product);
            await _itemRepository.SaveAsync(product);

            var part = CreateItem("PART-C", "部品C", ItemCategory.Part);
            await _itemRepository.SaveAsync(part);

            var bom = new Bom
            {
                ParentItemCode = "PRODUCT-Y",
                ChildItemCode = "PART-C",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                BaseQuantity = 1m,
                RequiredQuantity = 10m,
                DefectRate = 5m // 5%不良
            };
            await _bomRepository.SaveAsync(bom);

            // Act
            var children = await _bomRepository.FindByParentItemCodeAsync("PRODUCT-Y");

            // Assert: 10 × (1 + 0.05) = 10.5
            var result = children[0];
            var actualQuantity = result.RequiredQuantity * (1 + result.DefectRate / 100);
            actualQuantity.Should().Be(10.5m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 多階層のBOMを再帰展開できる()
        {
            // Arrange: 製品X -> 中間品N -> 部品A という3階層構造
            var productX = CreateItem("PRODUCT-X", "製品X", ItemCategory.Product);
            await _itemRepository.SaveAsync(productX);

            var intermediateN = CreateItem("INTER-N", "中間品N", ItemCategory.Intermediate);
            await _itemRepository.SaveAsync(intermediateN);

            var partA = CreateItem("PART-A", "部品A", ItemCategory.Part);
            await _itemRepository.SaveAsync(partA);

            // BOMを登録
            await _bomRepository.SaveAsync(CreateBom("PRODUCT-X", "INTER-N", 1));
            await _bomRepository.SaveAsync(CreateBom("INTER-N", "PART-A", 2));

            // Act
            var explosion = await _bomRepository.ExplodeAsync("PRODUCT-X", 10);

            // Assert
            explosion.Should().HaveCount(2);

            var level1 = explosion.First(e => e.Level == 1);
            level1.ChildItemCode.Should().Be("INTER-N");
            level1.TotalQuantity.Should().Be(10m);

            var level2 = explosion.First(e => e.Level == 2);
            level2.ChildItemCode.Should().Be("PART-A");
            level2.TotalQuantity.Should().Be(20m); // 10 * 1 * 2 = 20
        }
    }

    public class ReverseExplosion : BomRepositoryTests
    {
        public ReverseExplosion(PostgresFixture fixture)
            : base(fixture)
        {
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 子品目から親品目を逆引きできる()
        {
            // Arrange
            var productX = CreateItem("PRODUCT-X", "製品X", ItemCategory.Product);
            await _itemRepository.SaveAsync(productX);

            var productY = CreateItem("PRODUCT-Y", "製品Y", ItemCategory.Product);
            await _itemRepository.SaveAsync(productY);

            var partA = CreateItem("PART-A", "部品A", ItemCategory.Part);
            await _itemRepository.SaveAsync(partA);

            // 部品Aは製品Xと製品Yの両方で使用
            await _bomRepository.SaveAsync(CreateBom("PRODUCT-X", "PART-A", 2));
            await _bomRepository.SaveAsync(CreateBom("PRODUCT-Y", "PART-A", 3));

            // Act
            var parents = await _bomRepository.FindByChildItemCodeAsync("PART-A");

            // Assert
            parents.Should().HaveCount(2);
            parents.Select(b => b.ParentItemCode).Should().Contain(new[] { "PRODUCT-X", "PRODUCT-Y" });
        }
    }

    private static Item CreateItem(string itemCode, string itemName, ItemCategory category)
    {
        return new Item
        {
            ItemCode = itemCode,
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = itemName,
            ItemCategory = category
        };
    }

    private static Bom CreateBom(string parentCode, string childCode, int quantity)
    {
        return new Bom
        {
            ParentItemCode = parentCode,
            ChildItemCode = childCode,
            EffectiveFrom = new DateOnly(2025, 1, 1),
            BaseQuantity = 1m,
            RequiredQuantity = quantity
        };
    }
}
