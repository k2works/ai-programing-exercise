using FluentAssertions;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Inventory;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Location;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Services;

/// <summary>
/// 在庫サービステスト
/// </summary>
[Collection("Database")]
public class InventoryServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly InventoryService _inventoryService;
    private readonly IStockRepository _stockRepository;
    private readonly ILocationRepository _locationRepository;
    private readonly IItemRepository _itemRepository;

    public InventoryServiceTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _stockRepository = new StockRepository(fixture.ConnectionString);
        _locationRepository = new LocationRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _inventoryService = new InventoryService(_stockRepository, _itemRepository);

        // クリーンアップ
        _stockRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
        _locationRepository.DeleteAllAsync().Wait();
    }

    private async Task SetupTestDataAsync()
    {
        await _locationRepository.SaveAsync(new Location
        {
            LocationCode = "WH001",
            LocationName = "資材倉庫1",
            LocationType = LocationType.Warehouse
        });

        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "PROD001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "製品A",
            ItemCategory = ItemCategory.Product
        });
    }

    private async Task SetupStockDataAsync()
    {
        await SetupTestDataAsync();

        await _stockRepository.SaveAsync(new Stock
        {
            LocationCode = "WH001",
            ItemCode = "PROD001",
            StockQuantity = 100m,
            PassedQuantity = 95m,
            DefectiveQuantity = 3m,
            UninspectedQuantity = 2m
        });
    }

    public class 在庫の参照 : InventoryServiceTests
    {
        public 在庫の参照(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 場所と品目で在庫を取得できる()
        {
            // Arrange
            await SetupStockDataAsync();

            // Act
            var stock = await _inventoryService.GetStockAsync("WH001", "PROD001");

            // Assert
            stock.Should().NotBeNull();
            stock.StockQuantity.Should().Be(100m);
            stock.PassedQuantity.Should().Be(95m);
            stock.DefectiveQuantity.Should().Be(3m);
            stock.UninspectedQuantity.Should().Be(2m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 存在しない在庫は0として返す()
        {
            // Arrange
            await SetupTestDataAsync();

            // Act
            var stock = await _inventoryService.GetStockAsync("WH001", "NONEXISTENT");

            // Assert
            stock.StockQuantity.Should().Be(0m);
            stock.PassedQuantity.Should().Be(0m);
            stock.DefectiveQuantity.Should().Be(0m);
            stock.UninspectedQuantity.Should().Be(0m);
        }
    }

    public class 在庫の増減 : InventoryServiceTests
    {
        public 在庫の増減(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 在庫を増加できる()
        {
            // Arrange
            await SetupStockDataAsync();

            // Act
            await _inventoryService.IncreaseStockAsync(new StockChangeCommand
            {
                LocationCode = "WH001",
                ItemCode = "PROD001",
                Quantity = 50m,
                StockStatus = StockStatus.Passed
            });

            // Assert
            var stock = await _inventoryService.GetStockAsync("WH001", "PROD001");
            stock.StockQuantity.Should().Be(150m);
            stock.PassedQuantity.Should().Be(145m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 新規在庫を作成できる()
        {
            // Arrange
            await SetupTestDataAsync();

            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "NEWITEM",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "新規品目",
                ItemCategory = ItemCategory.Material
            });

            // Act
            await _inventoryService.IncreaseStockAsync(new StockChangeCommand
            {
                LocationCode = "WH001",
                ItemCode = "NEWITEM",
                Quantity = 100m,
                StockStatus = StockStatus.Passed
            });

            // Assert
            var stock = await _inventoryService.GetStockAsync("WH001", "NEWITEM");
            stock.StockQuantity.Should().Be(100m);
            stock.PassedQuantity.Should().Be(100m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 在庫を減少できる()
        {
            // Arrange
            await SetupStockDataAsync();

            // Act
            await _inventoryService.DecreaseStockAsync(new StockChangeCommand
            {
                LocationCode = "WH001",
                ItemCode = "PROD001",
                Quantity = 30m,
                StockStatus = StockStatus.Passed
            });

            // Assert
            var stock = await _inventoryService.GetStockAsync("WH001", "PROD001");
            stock.StockQuantity.Should().Be(70m);
            stock.PassedQuantity.Should().Be(65m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 在庫以上の数量を減少しようとするとエラー()
        {
            // Arrange
            await SetupStockDataAsync();

            // Act & Assert
            var act = async () => await _inventoryService.DecreaseStockAsync(new StockChangeCommand
            {
                LocationCode = "WH001",
                ItemCode = "PROD001",
                Quantity = 200m,
                StockStatus = StockStatus.Passed
            });

            await act.Should().ThrowAsync<InsufficientStockException>()
                .WithMessage("*在庫が不足しています*");
        }
    }

    public class 在庫状態の変更 : InventoryServiceTests
    {
        public 在庫状態の変更(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 未検査から合格への状態変更ができる()
        {
            // Arrange
            await SetupStockDataAsync();
            var initialStock = await _inventoryService.GetStockAsync("WH001", "PROD001");
            var initialUninspected = initialStock.UninspectedQuantity;
            var initialPassed = initialStock.PassedQuantity;

            // Act
            await _inventoryService.ChangeStockStatusAsync(new StockStatusChangeCommand
            {
                LocationCode = "WH001",
                ItemCode = "PROD001",
                Quantity = 2m,
                FromStatus = StockStatus.Uninspected,
                ToStatus = StockStatus.Passed
            });

            // Assert
            var stock = await _inventoryService.GetStockAsync("WH001", "PROD001");
            stock.UninspectedQuantity.Should().Be(initialUninspected - 2m);
            stock.PassedQuantity.Should().Be(initialPassed + 2m);
            stock.StockQuantity.Should().Be(100m); // 総数は変わらない
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 合格から不良への状態変更ができる()
        {
            // Arrange
            await SetupStockDataAsync();

            // Act
            await _inventoryService.ChangeStockStatusAsync(new StockStatusChangeCommand
            {
                LocationCode = "WH001",
                ItemCode = "PROD001",
                Quantity = 5m,
                FromStatus = StockStatus.Passed,
                ToStatus = StockStatus.Defective
            });

            // Assert
            var stock = await _inventoryService.GetStockAsync("WH001", "PROD001");
            stock.PassedQuantity.Should().Be(90m);
            stock.DefectiveQuantity.Should().Be(8m);
            stock.StockQuantity.Should().Be(100m); // 総数は変わらない
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 状態変更で在庫不足の場合エラー()
        {
            // Arrange
            await SetupStockDataAsync();

            // Act & Assert
            var act = async () => await _inventoryService.ChangeStockStatusAsync(new StockStatusChangeCommand
            {
                LocationCode = "WH001",
                ItemCode = "PROD001",
                Quantity = 10m,
                FromStatus = StockStatus.Uninspected,
                ToStatus = StockStatus.Passed
            });

            await act.Should().ThrowAsync<InsufficientStockException>()
                .WithMessage("*未検査の在庫が不足しています*");
        }
    }
}
