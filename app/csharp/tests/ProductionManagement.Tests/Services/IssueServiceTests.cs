using FluentAssertions;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Inventory;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Location;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Domain.Models.Process;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Services;

/// <summary>
/// 払出サービステスト
/// </summary>
[Collection("Database")]
public class IssueServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly IssueService _issueService;
    private readonly InventoryService _inventoryService;
    private readonly WorkOrderService _workOrderService;
    private readonly IIssueInstructionRepository _issueInstructionRepository;
    private readonly IIssueRepository _issueRepository;
    private readonly IStockRepository _stockRepository;
    private readonly ILocationRepository _locationRepository;
    private readonly IItemRepository _itemRepository;
    private readonly IOrderRepository _orderRepository;
    private readonly IWorkOrderRepository _workOrderRepository;
    private readonly IWorkOrderDetailRepository _workOrderDetailRepository;
    private readonly IRoutingRepository _routingRepository;
    private readonly IProcessRepository _processRepository;
    private readonly ILaborHoursRepository _laborHoursRepository;
    private readonly ICompletionResultRepository _completionResultRepository;
    private readonly ICompletionInspectionResultRepository _completionInspectionResultRepository;

    public IssueServiceTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _issueInstructionRepository = new IssueInstructionRepository(fixture.ConnectionString);
        _issueRepository = new IssueRepository(fixture.ConnectionString);
        _stockRepository = new StockRepository(fixture.ConnectionString);
        _locationRepository = new LocationRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _orderRepository = new OrderRepository(fixture.ConnectionString);
        _workOrderRepository = new WorkOrderRepository(fixture.ConnectionString);
        _workOrderDetailRepository = new WorkOrderDetailRepository(fixture.ConnectionString);
        _routingRepository = new RoutingRepository(fixture.ConnectionString);
        _processRepository = new ProcessRepository(fixture.ConnectionString);
        _laborHoursRepository = new LaborHoursRepository(fixture.ConnectionString);
        _completionResultRepository = new CompletionResultRepository(fixture.ConnectionString);
        _completionInspectionResultRepository = new CompletionInspectionResultRepository(fixture.ConnectionString);

        _inventoryService = new InventoryService(_stockRepository, _itemRepository);

        _issueService = new IssueService(
            _issueInstructionRepository,
            _issueRepository,
            _stockRepository,
            _inventoryService);

        _workOrderService = new WorkOrderService(
            _workOrderRepository,
            _workOrderDetailRepository,
            _orderRepository,
            _routingRepository);

        // クリーンアップ
        CleanupAsync().Wait();
    }

    private async Task CleanupAsync()
    {
        // FK制約の順序に従って削除
        await _issueRepository.DeleteAllDetailsAsync();
        await _issueRepository.DeleteAllAsync();
        await _issueInstructionRepository.DeleteAllDetailsAsync();
        await _issueInstructionRepository.DeleteAllAsync();
        await _stockRepository.DeleteAllAsync();
        await _laborHoursRepository.DeleteAllAsync();
        await _completionInspectionResultRepository.DeleteAllAsync();
        await _completionResultRepository.DeleteAllAsync();
        await _workOrderDetailRepository.DeleteAllAsync();
        await _workOrderRepository.DeleteAllAsync();
        await _routingRepository.DeleteAllAsync();
        await _processRepository.DeleteAllAsync();
        await _orderRepository.DeleteAllAsync();
        await _itemRepository.DeleteAllAsync();
        await _locationRepository.DeleteAllAsync();
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

        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "MAT001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "材料A",
            ItemCategory = ItemCategory.Material
        });

        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "MAT002",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "材料B",
            ItemCategory = ItemCategory.Material
        });

        await _orderRepository.SaveAsync(new Order
        {
            OrderNumber = "MO-IS-001",
            OrderType = OrderType.Manufacturing,
            ItemCode = "PROD001",
            StartDate = new DateOnly(2025, 1, 21),
            DueDate = new DateOnly(2025, 1, 25),
            PlanQuantity = 100m,
            LocationCode = "WH001",
            Status = PlanStatus.Confirmed
        });

        await _processRepository.SaveAsync(new Process
        {
            ProcessCode = "ASSEMBLY",
            ProcessName = "組立工程"
        });

        await _routingRepository.SaveAsync(new Routing
        {
            ItemCode = "PROD001",
            Sequence = 1,
            ProcessCode = "ASSEMBLY"
        });
    }

    private async Task SetupStockDataAsync()
    {
        await _stockRepository.SaveAsync(new Stock
        {
            LocationCode = "WH001",
            ItemCode = "MAT001",
            StockQuantity = 500m,
            PassedQuantity = 500m,
            DefectiveQuantity = 0m,
            UninspectedQuantity = 0m
        });

        await _stockRepository.SaveAsync(new Stock
        {
            LocationCode = "WH001",
            ItemCode = "MAT002",
            StockQuantity = 300m,
            PassedQuantity = 300m,
            DefectiveQuantity = 0m,
            UninspectedQuantity = 0m
        });
    }

    private async Task<WorkOrder> CreateTestWorkOrderAsync()
    {
        var createCommand = new WorkOrderCreateCommand
        {
            OrderNumber = "MO-IS-001",
            WorkOrderDate = new DateOnly(2025, 1, 20),
            LocationCode = "WH001",
            PlannedStartDate = new DateOnly(2025, 1, 21),
            PlannedEndDate = new DateOnly(2025, 1, 25)
        };
        var workOrder = await _workOrderService.CreateWorkOrderAsync(createCommand);

        await _workOrderService.StartWorkAsync(workOrder.WorkOrderNumber);
        return await _workOrderRepository.FindByWorkOrderNumberAsync(workOrder.WorkOrderNumber)
            ?? throw new InvalidOperationException("Work order not found");
    }

    public class 払出指示の作成 : IssueServiceTests
    {
        public 払出指示の作成(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 払出指示を作成できる()
        {
            // Arrange
            await SetupTestDataAsync();

            // Act
            var command = new IssueInstructionCommand
            {
                OrderNumber = "MO-IS-001",
                InstructionDate = new DateOnly(2025, 1, 20),
                LocationCode = "WH001",
                Remarks = "テスト払出指示",
                Details =
                [
                    new IssueInstructionDetailCommand
                    {
                        ItemCode = "MAT001",
                        RoutingSequence = 1,
                        IssueQuantity = 100m
                    },
                    new IssueInstructionDetailCommand
                    {
                        ItemCode = "MAT002",
                        RoutingSequence = 1,
                        IssueQuantity = 50m
                    }
                ]
            };

            var instruction = await _issueService.CreateIssueInstructionAsync(command);

            // Assert
            instruction.Should().NotBeNull();
            instruction.InstructionNumber.Should().StartWith("IS-202501-");
            instruction.OrderNumber.Should().Be("MO-IS-001");
            instruction.Details.Should().HaveCount(2);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 払出指示を検索できる()
        {
            // Arrange
            await SetupTestDataAsync();

            var command = new IssueInstructionCommand
            {
                OrderNumber = "MO-IS-001",
                InstructionDate = new DateOnly(2025, 1, 20),
                LocationCode = "WH001",
                Details =
                [
                    new IssueInstructionDetailCommand
                    {
                        ItemCode = "MAT001",
                        RoutingSequence = 1,
                        IssueQuantity = 100m
                    }
                ]
            };

            var created = await _issueService.CreateIssueInstructionAsync(command);

            // Act
            var found = await _issueService.FindIssueInstructionAsync(created.InstructionNumber);

            // Assert
            found.Should().NotBeNull();
            found!.InstructionNumber.Should().Be(created.InstructionNumber);
            found.Details.Should().HaveCount(1);
        }
    }

    public class 払出の実行 : IssueServiceTests
    {
        public 払出の実行(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 払出を実行できる()
        {
            // Arrange
            await SetupTestDataAsync();
            await SetupStockDataAsync();
            var workOrder = await CreateTestWorkOrderAsync();

            // Act
            var command = new IssueExecuteCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                RoutingSequence = 1,
                IssueDate = new DateOnly(2025, 1, 21),
                IssuerCode = "EMP001",
                LocationCode = "WH001",
                Details =
                [
                    new IssueDetailCommand
                    {
                        ItemCode = "MAT001",
                        IssueQuantity = 100m
                    },
                    new IssueDetailCommand
                    {
                        ItemCode = "MAT002",
                        IssueQuantity = 50m
                    }
                ]
            };

            var issue = await _issueService.ExecuteIssueAsync(command);

            // Assert
            issue.Should().NotBeNull();
            issue.IssueNumber.Should().StartWith("PO-202501-");
            issue.Details.Should().HaveCount(2);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 払出実行で在庫が減少する()
        {
            // Arrange
            await SetupTestDataAsync();
            await SetupStockDataAsync();
            var workOrder = await CreateTestWorkOrderAsync();

            var initialStock = await _inventoryService.GetStockAsync("WH001", "MAT001");

            // Act
            var command = new IssueExecuteCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                RoutingSequence = 1,
                IssueDate = new DateOnly(2025, 1, 21),
                IssuerCode = "EMP001",
                LocationCode = "WH001",
                Details =
                [
                    new IssueDetailCommand
                    {
                        ItemCode = "MAT001",
                        IssueQuantity = 100m
                    }
                ]
            };

            await _issueService.ExecuteIssueAsync(command);

            // Assert
            var afterStock = await _inventoryService.GetStockAsync("WH001", "MAT001");
            afterStock.StockQuantity.Should().Be(initialStock.StockQuantity - 100m);
            afterStock.PassedQuantity.Should().Be(initialStock.PassedQuantity - 100m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 在庫不足の場合は払出できない()
        {
            // Arrange
            await SetupTestDataAsync();
            await SetupStockDataAsync();
            var workOrder = await CreateTestWorkOrderAsync();

            // Act & Assert
            var command = new IssueExecuteCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                RoutingSequence = 1,
                IssueDate = new DateOnly(2025, 1, 21),
                IssuerCode = "EMP001",
                LocationCode = "WH001",
                Details =
                [
                    new IssueDetailCommand
                    {
                        ItemCode = "MAT001",
                        IssueQuantity = 1000m // 在庫以上の数量
                    }
                ]
            };

            var act = async () => await _issueService.ExecuteIssueAsync(command);
            await act.Should().ThrowAsync<InsufficientStockException>()
                .WithMessage("*在庫が不足しています*");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 払出を検索できる()
        {
            // Arrange
            await SetupTestDataAsync();
            await SetupStockDataAsync();
            var workOrder = await CreateTestWorkOrderAsync();

            var command = new IssueExecuteCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                RoutingSequence = 1,
                IssueDate = new DateOnly(2025, 1, 21),
                IssuerCode = "EMP001",
                LocationCode = "WH001",
                Details =
                [
                    new IssueDetailCommand
                    {
                        ItemCode = "MAT001",
                        IssueQuantity = 50m
                    }
                ]
            };

            var created = await _issueService.ExecuteIssueAsync(command);

            // Act
            var found = await _issueService.FindIssueAsync(created.IssueNumber);

            // Assert
            found.Should().NotBeNull();
            found!.IssueNumber.Should().Be(created.IssueNumber);
            found.Details.Should().HaveCount(1);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 作業指示番号で払出を検索できる()
        {
            // Arrange
            await SetupTestDataAsync();
            await SetupStockDataAsync();
            var workOrder = await CreateTestWorkOrderAsync();

            // 2回払出を実行
            var command1 = new IssueExecuteCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                RoutingSequence = 1,
                IssueDate = new DateOnly(2025, 1, 21),
                IssuerCode = "EMP001",
                LocationCode = "WH001",
                Details = [new IssueDetailCommand { ItemCode = "MAT001", IssueQuantity = 50m }]
            };
            await _issueService.ExecuteIssueAsync(command1);

            var command2 = new IssueExecuteCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                RoutingSequence = 1,
                IssueDate = new DateOnly(2025, 1, 22),
                IssuerCode = "EMP001",
                LocationCode = "WH001",
                Details = [new IssueDetailCommand { ItemCode = "MAT002", IssueQuantity = 30m }]
            };
            await _issueService.ExecuteIssueAsync(command2);

            // Act
            var issues = await _issueService.FindIssuesByWorkOrderAsync(workOrder.WorkOrderNumber);

            // Assert
            issues.Should().HaveCount(2);
        }
    }
}
