using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Cost;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Location;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Domain.Models.Process;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// 原価リポジトリテスト
/// </summary>
[Collection("Database")]
public class CostRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly ICostRepository _costRepository;
    private readonly IItemRepository _itemRepository;
    private readonly ILocationRepository _locationRepository;
    private readonly IOrderRepository _orderRepository;
    private readonly IWorkOrderRepository _workOrderRepository;
    private readonly IRequirementRepository _requirementRepository;
    private readonly IAllocationRepository _allocationRepository;

    public CostRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _costRepository = new CostRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _locationRepository = new LocationRepository(fixture.ConnectionString);
        _orderRepository = new OrderRepository(fixture.ConnectionString);
        _workOrderRepository = new WorkOrderRepository(fixture.ConnectionString);
        _requirementRepository = new RequirementRepository(fixture.ConnectionString);
        _allocationRepository = new AllocationRepository(fixture.ConnectionString);

        // クリーンアップ（FK制約の順序に従う）
        _costRepository.DeleteAllCostVariancesAsync().Wait();
        _costRepository.DeleteAllActualCostsAsync().Wait();
        _costRepository.DeleteAllStandardCostsAsync().Wait();
        _workOrderRepository.DeleteAllAsync().Wait();
        _allocationRepository.DeleteAllAsync().Wait();
        _requirementRepository.DeleteAllAsync().Wait();
        _orderRepository.DeleteAllAsync().Wait();
        _locationRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    private async Task SetupMasterDataAsync()
    {
        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "PROD001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "製品A",
            ItemCategory = ItemCategory.Product
        });

        await _locationRepository.SaveAsync(new Location
        {
            LocationCode = "WH001",
            LocationName = "倉庫A",
            LocationType = LocationType.Warehouse
        });
    }

    private async Task SetupWorkOrderDataAsync()
    {
        await SetupMasterDataAsync();

        // 製造オーダ作成
        await _orderRepository.SaveAsync(new Order
        {
            OrderNumber = "MO-202506-0001",
            OrderType = OrderType.Manufacturing,
            ItemCode = "PROD001",
            StartDate = new DateOnly(2025, 6, 1),
            DueDate = new DateOnly(2025, 6, 15),
            PlanQuantity = 100m,
            LocationCode = "WH001",
            Status = PlanStatus.Draft
        });

        // 作業指示作成
        await _workOrderRepository.SaveAsync(new WorkOrder
        {
            WorkOrderNumber = "WO-202506-0001",
            OrderNumber = "MO-202506-0001",
            WorkOrderDate = new DateOnly(2025, 6, 1),
            ItemCode = "PROD001",
            OrderQuantity = 100m,
            LocationCode = "WH001",
            PlannedStartDate = new DateOnly(2025, 6, 1),
            PlannedEndDate = new DateOnly(2025, 6, 15),
            Status = WorkOrderStatus.NotStarted
        });
    }

    public class 標準原価 : CostRepositoryTests
    {
        public 標準原価(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 標準原価を登録して取得できる()
        {
            // Arrange
            await SetupMasterDataAsync();
            var standardCost = new StandardCost
            {
                ItemCode = "PROD001",
                EffectiveStartDate = new DateOnly(2025, 1, 1),
                EffectiveEndDate = null,
                StandardMaterialCost = 1000.00m,
                StandardLaborCost = 500.00m,
                StandardExpense = 200.00m,
                StandardManufacturingCost = 1700.00m
            };

            // Act
            await _costRepository.SaveStandardCostAsync(standardCost);
            var result = await _costRepository.FindStandardCostByItemCodeAsync("PROD001", new DateOnly(2025, 6, 1));

            // Assert
            result.Should().NotBeNull();
            result!.ItemCode.Should().Be("PROD001");
            result.StandardMaterialCost.Should().Be(1000.00m);
            result.StandardLaborCost.Should().Be(500.00m);
            result.StandardExpense.Should().Be(200.00m);
            result.StandardManufacturingCost.Should().Be(1700.00m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 適用期間で有効な標準原価を取得できる()
        {
            // Arrange
            await SetupMasterDataAsync();

            // 旧バージョン
            await _costRepository.SaveStandardCostAsync(new StandardCost
            {
                ItemCode = "PROD001",
                EffectiveStartDate = new DateOnly(2025, 1, 1),
                EffectiveEndDate = new DateOnly(2025, 5, 31),
                StandardMaterialCost = 900.00m,
                StandardLaborCost = 450.00m,
                StandardExpense = 180.00m,
                StandardManufacturingCost = 1530.00m
            });

            // 新バージョン
            await _costRepository.SaveStandardCostAsync(new StandardCost
            {
                ItemCode = "PROD001",
                EffectiveStartDate = new DateOnly(2025, 6, 1),
                EffectiveEndDate = null,
                StandardMaterialCost = 1000.00m,
                StandardLaborCost = 500.00m,
                StandardExpense = 200.00m,
                StandardManufacturingCost = 1700.00m
            });

            // Act
            var resultOld = await _costRepository.FindStandardCostByItemCodeAsync("PROD001", new DateOnly(2025, 3, 1));
            var resultNew = await _costRepository.FindStandardCostByItemCodeAsync("PROD001", new DateOnly(2025, 7, 1));

            // Assert
            resultOld.Should().NotBeNull();
            resultOld!.StandardManufacturingCost.Should().Be(1530.00m);

            resultNew.Should().NotBeNull();
            resultNew!.StandardManufacturingCost.Should().Be(1700.00m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 品目コードで全標準原価を取得できる()
        {
            // Arrange
            await SetupMasterDataAsync();

            await _costRepository.SaveStandardCostAsync(new StandardCost
            {
                ItemCode = "PROD001",
                EffectiveStartDate = new DateOnly(2025, 1, 1),
                EffectiveEndDate = new DateOnly(2025, 5, 31),
                StandardMaterialCost = 900.00m,
                StandardLaborCost = 450.00m,
                StandardExpense = 180.00m,
                StandardManufacturingCost = 1530.00m
            });

            await _costRepository.SaveStandardCostAsync(new StandardCost
            {
                ItemCode = "PROD001",
                EffectiveStartDate = new DateOnly(2025, 6, 1),
                EffectiveEndDate = null,
                StandardMaterialCost = 1000.00m,
                StandardLaborCost = 500.00m,
                StandardExpense = 200.00m,
                StandardManufacturingCost = 1700.00m
            });

            // Act
            var result = await _costRepository.FindAllStandardCostsByItemCodeAsync("PROD001");

            // Assert
            result.Should().HaveCount(2);
        }
    }

    public class 実際原価 : CostRepositoryTests
    {
        public 実際原価(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 実際原価を登録して取得できる()
        {
            // Arrange
            await SetupWorkOrderDataAsync();
            var actualCost = new ActualCost
            {
                WorkOrderNumber = "WO-202506-0001",
                ItemCode = "PROD001",
                CompletedQuantity = 100m,
                ActualMaterialCost = 105000.00m,
                ActualLaborCost = 52000.00m,
                ActualExpense = 21000.00m,
                ActualManufacturingCost = 178000.00m,
                UnitCost = 1780.0000m
            };

            // Act
            await _costRepository.SaveActualCostAsync(actualCost);
            var result = await _costRepository.FindActualCostByWorkOrderNumberAsync("WO-202506-0001");

            // Assert
            result.Should().NotBeNull();
            result!.WorkOrderNumber.Should().Be("WO-202506-0001");
            result.ItemCode.Should().Be("PROD001");
            result.CompletedQuantity.Should().Be(100m);
            result.ActualMaterialCost.Should().Be(105000.00m);
            result.ActualLaborCost.Should().Be(52000.00m);
            result.ActualExpense.Should().Be(21000.00m);
            result.ActualManufacturingCost.Should().Be(178000.00m);
            result.UnitCost.Should().Be(1780.0000m);
        }
    }

    public class 原価差異 : CostRepositoryTests
    {
        public 原価差異(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 原価差異を登録して取得できる()
        {
            // Arrange
            await SetupWorkOrderDataAsync();
            var variance = new CostVariance
            {
                WorkOrderNumber = "WO-202506-0001",
                ItemCode = "PROD001",
                MaterialCostVariance = 5000.00m,
                LaborCostVariance = 2000.00m,
                ExpenseVariance = 1000.00m,
                TotalVariance = 8000.00m
            };

            // Act
            await _costRepository.SaveCostVarianceAsync(variance);
            var result = await _costRepository.FindCostVarianceByWorkOrderNumberAsync("WO-202506-0001");

            // Assert
            result.Should().NotBeNull();
            result!.WorkOrderNumber.Should().Be("WO-202506-0001");
            result.ItemCode.Should().Be("PROD001");
            result.MaterialCostVariance.Should().Be(5000.00m);
            result.LaborCostVariance.Should().Be(2000.00m);
            result.ExpenseVariance.Should().Be(1000.00m);
            result.TotalVariance.Should().Be(8000.00m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task マイナス差異も登録できる()
        {
            // Arrange
            await SetupWorkOrderDataAsync();
            var variance = new CostVariance
            {
                WorkOrderNumber = "WO-202506-0001",
                ItemCode = "PROD001",
                MaterialCostVariance = -3000.00m,
                LaborCostVariance = -1500.00m,
                ExpenseVariance = -500.00m,
                TotalVariance = -5000.00m
            };

            // Act
            await _costRepository.SaveCostVarianceAsync(variance);
            var result = await _costRepository.FindCostVarianceByWorkOrderNumberAsync("WO-202506-0001");

            // Assert
            result.Should().NotBeNull();
            result!.TotalVariance.Should().Be(-5000.00m);
        }
    }
}
