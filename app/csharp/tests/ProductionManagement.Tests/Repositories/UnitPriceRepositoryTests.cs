using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// 単価マスタリポジトリテスト
/// </summary>
[Collection("Database")]
public class UnitPriceRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly IUnitPriceRepository _unitPriceRepository;
    private readonly IItemRepository _itemRepository;
    private readonly ISupplierRepository _supplierRepository;

    public UnitPriceRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _unitPriceRepository = new UnitPriceRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _supplierRepository = new SupplierRepository(fixture.ConnectionString);

        _unitPriceRepository.DeleteAllAsync().Wait();
        _supplierRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    public class Registration : UnitPriceRepositoryTests
    {
        public Registration(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 単価を登録できる()
        {
            // Arrange
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料A",
                ItemCategory = ItemCategory.Material
            });

            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "テスト仕入先",
                SupplierType = SupplierType.Vendor
            });

            // Act
            var unitPrice = new UnitPrice
            {
                ItemCode = "MAT-001",
                SupplierCode = "SUP-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                Price = 1000m
            };
            await _unitPriceRepository.SaveAsync(unitPrice);

            // Assert
            unitPrice.Id.Should().NotBe(0);

            var result = await _unitPriceRepository.FindEffectiveUnitPriceAsync(
                "MAT-001", "SUP-001", new DateOnly(2025, 1, 15));
            result.Should().NotBeNull();
            result!.Price.Should().Be(1000m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task ロット単位数を指定して登録できる()
        {
            // Arrange
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料B",
                ItemCategory = ItemCategory.Material
            });

            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "テスト仕入先2",
                SupplierType = SupplierType.Vendor
            });

            // Act
            var unitPrice = new UnitPrice
            {
                ItemCode = "MAT-002",
                SupplierCode = "SUP-002",
                LotUnitQuantity = 100m,
                EffectiveFrom = new DateOnly(2025, 1, 1),
                Price = 500m
            };
            await _unitPriceRepository.SaveAsync(unitPrice);

            // Assert
            var result = await _unitPriceRepository.FindEffectiveUnitPriceAsync(
                "MAT-002", "SUP-002", new DateOnly(2025, 1, 15));
            result.Should().NotBeNull();
            result!.LotUnitQuantity.Should().Be(100m);
            result.Price.Should().Be(500m);
        }
    }

    public class EffectiveDateQuery : UnitPriceRepositoryTests
    {
        public EffectiveDateQuery(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 基準日で有効な単価を取得できる()
        {
            // Arrange
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料C",
                ItemCategory = ItemCategory.Material
            });

            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "テスト仕入先3",
                SupplierType = SupplierType.Vendor
            });

            // 1月から3月末まで有効な単価
            await _unitPriceRepository.SaveAsync(new UnitPrice
            {
                ItemCode = "MAT-003",
                SupplierCode = "SUP-003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                EffectiveTo = new DateOnly(2025, 3, 31),
                Price = 1000m
            });

            // 4月から有効な単価
            await _unitPriceRepository.SaveAsync(new UnitPrice
            {
                ItemCode = "MAT-003",
                SupplierCode = "SUP-003",
                EffectiveFrom = new DateOnly(2025, 4, 1),
                Price = 1200m
            });

            // Act & Assert: 2月時点では旧単価
            var resultFeb = await _unitPriceRepository.FindEffectiveUnitPriceAsync(
                "MAT-003", "SUP-003", new DateOnly(2025, 2, 15));
            resultFeb.Should().NotBeNull();
            resultFeb!.Price.Should().Be(1000m);

            // Act & Assert: 5月時点では新単価
            var resultMay = await _unitPriceRepository.FindEffectiveUnitPriceAsync(
                "MAT-003", "SUP-003", new DateOnly(2025, 5, 15));
            resultMay.Should().NotBeNull();
            resultMay!.Price.Should().Be(1200m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 有効な単価がない場合はNullを返す()
        {
            // Act
            var result = await _unitPriceRepository.FindEffectiveUnitPriceAsync(
                "NONEXISTENT", "NONEXISTENT", new DateOnly(2025, 1, 15));

            // Assert
            result.Should().BeNull();
        }
    }
}
