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
/// 棚卸サービステスト
/// </summary>
[Collection("Database")]
public class StocktakingServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly StocktakingService _stocktakingService;
    private readonly IStocktakingRepository _stocktakingRepository;
    private readonly IStockAdjustmentRepository _stockAdjustmentRepository;
    private readonly IStockRepository _stockRepository;
    private readonly ILocationRepository _locationRepository;
    private readonly IItemRepository _itemRepository;

    public StocktakingServiceTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _stocktakingRepository = new StocktakingRepository(fixture.ConnectionString);
        _stockAdjustmentRepository = new StockAdjustmentRepository(fixture.ConnectionString);
        _stockRepository = new StockRepository(fixture.ConnectionString);
        _locationRepository = new LocationRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _stocktakingService = new StocktakingService(
            _stocktakingRepository,
            _stockAdjustmentRepository,
            _stockRepository);

        // クリーンアップ
        _stockAdjustmentRepository.DeleteAllAsync().Wait();
        _stocktakingRepository.DeleteAllDetailsAsync().Wait();
        _stocktakingRepository.DeleteAllAsync().Wait();
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
            ItemCode = "MAT001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "資材A",
            ItemCategory = ItemCategory.Material
        });

        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "MAT002",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "資材B",
            ItemCategory = ItemCategory.Material
        });
    }

    private async Task SetupStockDataAsync()
    {
        await SetupTestDataAsync();

        await _stockRepository.SaveAsync(new Stock
        {
            LocationCode = "WH001",
            ItemCode = "MAT001",
            StockQuantity = 100m,
            PassedQuantity = 100m,
            DefectiveQuantity = 0m,
            UninspectedQuantity = 0m
        });

        await _stockRepository.SaveAsync(new Stock
        {
            LocationCode = "WH001",
            ItemCode = "MAT002",
            StockQuantity = 50m,
            PassedQuantity = 50m,
            DefectiveQuantity = 0m,
            UninspectedQuantity = 0m
        });
    }

    public class 棚卸表発行 : StocktakingServiceTests
    {
        public 棚卸表発行(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 棚卸表を発行できる()
        {
            // Arrange
            await SetupStockDataAsync();
            var command = new StocktakingIssueCommand
            {
                LocationCode = "WH001",
                StocktakingDate = new DateOnly(2025, 6, 15)
            };

            // Act
            var result = await _stocktakingService.IssueStocktakingSheetAsync(command);

            // Assert
            result.Should().NotBeNull();
            result.StocktakingNumber.Should().StartWith("ST-202506-");
            result.LocationCode.Should().Be("WH001");
            result.StocktakingDate.Should().Be(new DateOnly(2025, 6, 15));
            result.Status.Should().Be(StocktakingStatus.Issued);
            result.Details.Should().HaveCount(2);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 棚卸明細に帳簿数量が設定される()
        {
            // Arrange
            await SetupStockDataAsync();
            var command = new StocktakingIssueCommand
            {
                LocationCode = "WH001",
                StocktakingDate = new DateOnly(2025, 6, 15)
            };

            // Act
            var result = await _stocktakingService.IssueStocktakingSheetAsync(command);

            // Assert
            var mat001Detail = result.Details!.FirstOrDefault(d => d.ItemCode == "MAT001");
            mat001Detail.Should().NotBeNull();
            mat001Detail!.BookQuantity.Should().Be(100m);
            mat001Detail.ActualQuantity.Should().BeNull();

            var mat002Detail = result.Details!.FirstOrDefault(d => d.ItemCode == "MAT002");
            mat002Detail.Should().NotBeNull();
            mat002Detail!.BookQuantity.Should().Be(50m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 棚卸番号が連番で採番される()
        {
            // Arrange
            await SetupStockDataAsync();
            var date = new DateOnly(2025, 6, 15);
            var command1 = new StocktakingIssueCommand
            {
                LocationCode = "WH001",
                StocktakingDate = date
            };

            // 別の場所用のテストデータ
            await _locationRepository.SaveAsync(new Location
            {
                LocationCode = "WH002",
                LocationName = "資材倉庫2",
                LocationType = LocationType.Warehouse
            });

            await _stockRepository.SaveAsync(new Stock
            {
                LocationCode = "WH002",
                ItemCode = "MAT001",
                StockQuantity = 30m,
                PassedQuantity = 30m,
                DefectiveQuantity = 0m,
                UninspectedQuantity = 0m
            });

            var command2 = new StocktakingIssueCommand
            {
                LocationCode = "WH002",
                StocktakingDate = date
            };

            // Act
            var result1 = await _stocktakingService.IssueStocktakingSheetAsync(command1);
            var result2 = await _stocktakingService.IssueStocktakingSheetAsync(command2);

            // Assert
            result1.StocktakingNumber.Should().Be("ST-202506-0001");
            result2.StocktakingNumber.Should().Be("ST-202506-0002");
        }
    }

    public class 実棚数量入力 : StocktakingServiceTests
    {
        public 実棚数量入力(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 実棚数量を入力できる()
        {
            // Arrange
            await SetupStockDataAsync();
            var issueCommand = new StocktakingIssueCommand
            {
                LocationCode = "WH001",
                StocktakingDate = new DateOnly(2025, 6, 15)
            };
            var stocktaking = await _stocktakingService.IssueStocktakingSheetAsync(issueCommand);

            var inputCommand = new ActualCountInputCommand
            {
                StocktakingNumber = stocktaking.StocktakingNumber,
                Details = new List<ActualCountDetailInput>
                {
                    new() { ItemCode = "MAT001", ActualQuantity = 95m },
                    new() { ItemCode = "MAT002", ActualQuantity = 55m }
                }
            };

            // Act
            await _stocktakingService.InputActualCountAsync(inputCommand);

            // Assert
            var updated = await _stocktakingRepository.FindByStocktakingNumberAsync(stocktaking.StocktakingNumber);
            updated!.Status.Should().Be(StocktakingStatus.Entered);

            var details = await _stocktakingRepository.FindDetailsByStocktakingNumberAsync(stocktaking.StocktakingNumber);
            var mat001Detail = details.First(d => d.ItemCode == "MAT001");
            mat001Detail.ActualQuantity.Should().Be(95m);
            mat001Detail.DifferenceQuantity.Should().Be(-5m); // 実棚95 - 帳簿100 = -5

            var mat002Detail = details.First(d => d.ItemCode == "MAT002");
            mat002Detail.ActualQuantity.Should().Be(55m);
            mat002Detail.DifferenceQuantity.Should().Be(5m); // 実棚55 - 帳簿50 = 5
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 存在しない棚卸番号でエラー()
        {
            // Arrange
            var inputCommand = new ActualCountInputCommand
            {
                StocktakingNumber = "NONEXISTENT",
                Details = new List<ActualCountDetailInput>()
            };

            // Act & Assert
            var act = async () => await _stocktakingService.InputActualCountAsync(inputCommand);
            await act.Should().ThrowAsync<ResourceNotFoundException>()
                .WithMessage("*棚卸データが見つかりません*");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 発行済み以外のステータスでエラー()
        {
            // Arrange
            await SetupStockDataAsync();
            var issueCommand = new StocktakingIssueCommand
            {
                LocationCode = "WH001",
                StocktakingDate = new DateOnly(2025, 6, 15)
            };
            var stocktaking = await _stocktakingService.IssueStocktakingSheetAsync(issueCommand);

            // 入力済みに更新
            await _stocktakingRepository.UpdateStatusAsync(stocktaking.StocktakingNumber, StocktakingStatus.Entered);

            var inputCommand = new ActualCountInputCommand
            {
                StocktakingNumber = stocktaking.StocktakingNumber,
                Details = new List<ActualCountDetailInput>()
            };

            // Act & Assert
            var act = async () => await _stocktakingService.InputActualCountAsync(inputCommand);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("*発行済みの棚卸データのみ入力可能です*");
        }
    }

    public class 棚卸確定 : StocktakingServiceTests
    {
        public 棚卸確定(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 棚卸を確定できる()
        {
            // Arrange
            await SetupStockDataAsync();
            var issueCommand = new StocktakingIssueCommand
            {
                LocationCode = "WH001",
                StocktakingDate = new DateOnly(2025, 6, 15)
            };
            var stocktaking = await _stocktakingService.IssueStocktakingSheetAsync(issueCommand);

            var inputCommand = new ActualCountInputCommand
            {
                StocktakingNumber = stocktaking.StocktakingNumber,
                Details = new List<ActualCountDetailInput>
                {
                    new() { ItemCode = "MAT001", ActualQuantity = 95m },
                    new() { ItemCode = "MAT002", ActualQuantity = 55m }
                }
            };
            await _stocktakingService.InputActualCountAsync(inputCommand);

            var confirmCommand = new StocktakingConfirmCommand
            {
                StocktakingNumber = stocktaking.StocktakingNumber,
                AdjusterCode = "EMP001",
                AdjustmentReasonCode = "STK"
            };

            // Act
            await _stocktakingService.ConfirmStocktakingAsync(confirmCommand);

            // Assert
            var updated = await _stocktakingRepository.FindByStocktakingNumberAsync(stocktaking.StocktakingNumber);
            updated!.Status.Should().Be(StocktakingStatus.Confirmed);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 差異がある場合に在庫調整データが作成される()
        {
            // Arrange
            await SetupStockDataAsync();
            var issueCommand = new StocktakingIssueCommand
            {
                LocationCode = "WH001",
                StocktakingDate = new DateOnly(2025, 6, 15)
            };
            var stocktaking = await _stocktakingService.IssueStocktakingSheetAsync(issueCommand);

            var inputCommand = new ActualCountInputCommand
            {
                StocktakingNumber = stocktaking.StocktakingNumber,
                Details = new List<ActualCountDetailInput>
                {
                    new() { ItemCode = "MAT001", ActualQuantity = 95m },
                    new() { ItemCode = "MAT002", ActualQuantity = 55m }
                }
            };
            await _stocktakingService.InputActualCountAsync(inputCommand);

            var confirmCommand = new StocktakingConfirmCommand
            {
                StocktakingNumber = stocktaking.StocktakingNumber,
                AdjusterCode = "EMP001",
                AdjustmentReasonCode = "STK"
            };

            // Act
            await _stocktakingService.ConfirmStocktakingAsync(confirmCommand);

            // Assert
            var adjustments = await _stockAdjustmentRepository.FindByStocktakingNumberAsync(stocktaking.StocktakingNumber);
            adjustments.Should().HaveCount(2);

            var mat001Adj = adjustments.First(a => a.ItemCode == "MAT001");
            mat001Adj.AdjustmentQuantity.Should().Be(-5m);
            mat001Adj.AdjusterCode.Should().Be("EMP001");
            mat001Adj.ReasonCode.Should().Be("STK");

            var mat002Adj = adjustments.First(a => a.ItemCode == "MAT002");
            mat002Adj.AdjustmentQuantity.Should().Be(5m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 在庫調整により在庫数量が更新される()
        {
            // Arrange
            await SetupStockDataAsync();
            var issueCommand = new StocktakingIssueCommand
            {
                LocationCode = "WH001",
                StocktakingDate = new DateOnly(2025, 6, 15)
            };
            var stocktaking = await _stocktakingService.IssueStocktakingSheetAsync(issueCommand);

            var inputCommand = new ActualCountInputCommand
            {
                StocktakingNumber = stocktaking.StocktakingNumber,
                Details = new List<ActualCountDetailInput>
                {
                    new() { ItemCode = "MAT001", ActualQuantity = 95m },  // 100 -> 95 (-5)
                    new() { ItemCode = "MAT002", ActualQuantity = 55m }   // 50 -> 55 (+5)
                }
            };
            await _stocktakingService.InputActualCountAsync(inputCommand);

            var confirmCommand = new StocktakingConfirmCommand
            {
                StocktakingNumber = stocktaking.StocktakingNumber,
                AdjusterCode = "EMP001",
                AdjustmentReasonCode = "STK"
            };

            // Act
            await _stocktakingService.ConfirmStocktakingAsync(confirmCommand);

            // Assert
            var mat001Stock = await _stockRepository.FindByLocationAndItemAsync("WH001", "MAT001");
            mat001Stock!.StockQuantity.Should().Be(95m);
            mat001Stock.PassedQuantity.Should().Be(95m);

            var mat002Stock = await _stockRepository.FindByLocationAndItemAsync("WH001", "MAT002");
            mat002Stock!.StockQuantity.Should().Be(55m);
            mat002Stock.PassedQuantity.Should().Be(55m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 差異がない場合は在庫調整が作成されない()
        {
            // Arrange
            await SetupStockDataAsync();
            var issueCommand = new StocktakingIssueCommand
            {
                LocationCode = "WH001",
                StocktakingDate = new DateOnly(2025, 6, 15)
            };
            var stocktaking = await _stocktakingService.IssueStocktakingSheetAsync(issueCommand);

            var inputCommand = new ActualCountInputCommand
            {
                StocktakingNumber = stocktaking.StocktakingNumber,
                Details = new List<ActualCountDetailInput>
                {
                    new() { ItemCode = "MAT001", ActualQuantity = 100m },  // 差異なし
                    new() { ItemCode = "MAT002", ActualQuantity = 50m }    // 差異なし
                }
            };
            await _stocktakingService.InputActualCountAsync(inputCommand);

            var confirmCommand = new StocktakingConfirmCommand
            {
                StocktakingNumber = stocktaking.StocktakingNumber,
                AdjusterCode = "EMP001",
                AdjustmentReasonCode = "STK"
            };

            // Act
            await _stocktakingService.ConfirmStocktakingAsync(confirmCommand);

            // Assert
            var adjustments = await _stockAdjustmentRepository.FindByStocktakingNumberAsync(stocktaking.StocktakingNumber);
            adjustments.Should().BeEmpty();
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 入力済み以外のステータスでエラー()
        {
            // Arrange
            await SetupStockDataAsync();
            var issueCommand = new StocktakingIssueCommand
            {
                LocationCode = "WH001",
                StocktakingDate = new DateOnly(2025, 6, 15)
            };
            var stocktaking = await _stocktakingService.IssueStocktakingSheetAsync(issueCommand);

            var confirmCommand = new StocktakingConfirmCommand
            {
                StocktakingNumber = stocktaking.StocktakingNumber,
                AdjusterCode = "EMP001",
                AdjustmentReasonCode = "STK"
            };

            // Act & Assert
            var act = async () => await _stocktakingService.ConfirmStocktakingAsync(confirmCommand);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("*入力済みの棚卸データのみ確定可能です*");
        }
    }
}
