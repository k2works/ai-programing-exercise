using FluentAssertions;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Location;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Domain.Models.Process;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Services;

/// <summary>
/// 作業指示サービステスト
/// </summary>
[Collection("Database")]
public class WorkOrderServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly WorkOrderService _workOrderService;
    private readonly IWorkOrderRepository _workOrderRepository;
    private readonly IWorkOrderDetailRepository _workOrderDetailRepository;
    private readonly IOrderRepository _orderRepository;
    private readonly IRoutingRepository _routingRepository;
    private readonly IProcessRepository _processRepository;
    private readonly ILocationRepository _locationRepository;
    private readonly IItemRepository _itemRepository;
    private readonly IAllocationRepository _allocationRepository;
    private readonly IRequirementRepository _requirementRepository;
    private readonly ICompletionInspectionResultRepository _completionInspectionResultRepository;
    private readonly ICompletionResultRepository _completionResultRepository;

    public WorkOrderServiceTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _workOrderRepository = new WorkOrderRepository(fixture.ConnectionString);
        _workOrderDetailRepository = new WorkOrderDetailRepository(fixture.ConnectionString);
        _orderRepository = new OrderRepository(fixture.ConnectionString);
        _routingRepository = new RoutingRepository(fixture.ConnectionString);
        _processRepository = new ProcessRepository(fixture.ConnectionString);
        _locationRepository = new LocationRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _allocationRepository = new AllocationRepository(fixture.ConnectionString);
        _requirementRepository = new RequirementRepository(fixture.ConnectionString);
        _completionInspectionResultRepository = new CompletionInspectionResultRepository(fixture.ConnectionString);
        _completionResultRepository = new CompletionResultRepository(fixture.ConnectionString);

        _workOrderService = new WorkOrderService(
            _workOrderRepository,
            _workOrderDetailRepository,
            _orderRepository,
            _routingRepository);

        // FK制約の順序に従って削除（完成実績 → 作業指示 → 依存テーブル）
        _completionInspectionResultRepository.DeleteAllAsync().Wait();
        _completionResultRepository.DeleteAllAsync().Wait();
        _workOrderDetailRepository.DeleteAllAsync().Wait();
        _workOrderRepository.DeleteAllAsync().Wait();
        _allocationRepository.DeleteAllAsync().Wait();
        _requirementRepository.DeleteAllAsync().Wait();
        _routingRepository.DeleteAllAsync().Wait();
        _processRepository.DeleteAllAsync().Wait();
        _orderRepository.DeleteAllAsync().Wait();
        _locationRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    public class 作業指示の作成 : WorkOrderServiceTests
    {
        public 作業指示の作成(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task オーダ情報から作業指示を作成できる()
        {
            // Arrange: マスタデータを準備
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "PROD-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品A",
                ItemCategory = ItemCategory.Product
            });

            await _locationRepository.SaveAsync(new Location
            {
                LocationCode = "LINE-001",
                LocationName = "製造ライン1",
                LocationType = LocationType.Manufacturing
            });

            await _processRepository.SaveAsync(new Process
            {
                ProcessCode = "PRESS",
                ProcessName = "プレス加工"
            });

            await _processRepository.SaveAsync(new Process
            {
                ProcessCode = "ASSEMBLY",
                ProcessName = "組立"
            });

            await _routingRepository.SaveAsync(new Routing
            {
                ItemCode = "PROD-001",
                Sequence = 1,
                ProcessCode = "PRESS"
            });

            await _routingRepository.SaveAsync(new Routing
            {
                ItemCode = "PROD-001",
                Sequence = 2,
                ProcessCode = "ASSEMBLY"
            });

            await _orderRepository.SaveAsync(new Order
            {
                OrderNumber = "MO-2025-001",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-001",
                StartDate = new DateOnly(2025, 1, 21),
                DueDate = new DateOnly(2025, 1, 25),
                PlanQuantity = 100m,
                LocationCode = "LINE-001",
                Status = PlanStatus.Confirmed
            });

            // Act: 作業指示を作成
            var command = new WorkOrderCreateCommand
            {
                OrderNumber = "MO-2025-001",
                WorkOrderDate = new DateOnly(2025, 1, 20),
                LocationCode = "LINE-001",
                PlannedStartDate = new DateOnly(2025, 1, 21),
                PlannedEndDate = new DateOnly(2025, 1, 25)
            };

            var workOrder = await _workOrderService.CreateWorkOrderAsync(command);

            // Assert
            workOrder.Should().NotBeNull();
            workOrder.WorkOrderNumber.Should().StartWith("WO-202501-");
            workOrder.ItemCode.Should().Be("PROD-001");
            workOrder.OrderQuantity.Should().Be(100m);
            workOrder.Status.Should().Be(WorkOrderStatus.NotStarted);
            workOrder.Details.Should().HaveCount(2);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 工程表の工順に従って明細が作成される()
        {
            // Arrange
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "PROD-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品B",
                ItemCategory = ItemCategory.Product
            });

            await _locationRepository.SaveAsync(new Location
            {
                LocationCode = "LINE-002",
                LocationName = "製造ライン2",
                LocationType = LocationType.Manufacturing
            });

            await _processRepository.SaveAsync(new Process
            {
                ProcessCode = "CUT",
                ProcessName = "切断"
            });

            await _processRepository.SaveAsync(new Process
            {
                ProcessCode = "WELD",
                ProcessName = "溶接"
            });

            await _processRepository.SaveAsync(new Process
            {
                ProcessCode = "INSPECT",
                ProcessName = "検査"
            });

            await _routingRepository.SaveAsync(new Routing
            {
                ItemCode = "PROD-002",
                Sequence = 1,
                ProcessCode = "CUT"
            });

            await _routingRepository.SaveAsync(new Routing
            {
                ItemCode = "PROD-002",
                Sequence = 2,
                ProcessCode = "WELD"
            });

            await _routingRepository.SaveAsync(new Routing
            {
                ItemCode = "PROD-002",
                Sequence = 3,
                ProcessCode = "INSPECT"
            });

            await _orderRepository.SaveAsync(new Order
            {
                OrderNumber = "MO-2025-002",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-002",
                StartDate = new DateOnly(2025, 1, 21),
                DueDate = new DateOnly(2025, 1, 25),
                PlanQuantity = 50m,
                LocationCode = "LINE-002",
                Status = PlanStatus.Confirmed
            });

            // Act
            var command = new WorkOrderCreateCommand
            {
                OrderNumber = "MO-2025-002",
                WorkOrderDate = new DateOnly(2025, 1, 20),
                LocationCode = "LINE-002",
                PlannedStartDate = new DateOnly(2025, 1, 21),
                PlannedEndDate = new DateOnly(2025, 1, 25)
            };

            var workOrder = await _workOrderService.CreateWorkOrderAsync(command);

            // Assert: 工順の順序を確認
            workOrder.Details.Should().HaveCount(3);
            workOrder.Details[0].Sequence.Should().Be(1);
            workOrder.Details[0].ProcessCode.Should().Be("CUT");
            workOrder.Details[1].Sequence.Should().Be(2);
            workOrder.Details[1].ProcessCode.Should().Be("WELD");
            workOrder.Details[2].Sequence.Should().Be(3);
            workOrder.Details[2].ProcessCode.Should().Be("INSPECT");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 工程表が存在しない品目はエラー()
        {
            // Arrange
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "PROD-003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品C",
                ItemCategory = ItemCategory.Product
            });

            await _locationRepository.SaveAsync(new Location
            {
                LocationCode = "LINE-003",
                LocationName = "製造ライン3",
                LocationType = LocationType.Manufacturing
            });

            // 工程表は登録しない

            await _orderRepository.SaveAsync(new Order
            {
                OrderNumber = "MO-2025-003",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-003",
                StartDate = new DateOnly(2025, 1, 21),
                DueDate = new DateOnly(2025, 1, 25),
                PlanQuantity = 30m,
                LocationCode = "LINE-003",
                Status = PlanStatus.Confirmed
            });

            // Act & Assert
            var command = new WorkOrderCreateCommand
            {
                OrderNumber = "MO-2025-003",
                WorkOrderDate = new DateOnly(2025, 1, 20),
                LocationCode = "LINE-003",
                PlannedStartDate = new DateOnly(2025, 1, 21),
                PlannedEndDate = new DateOnly(2025, 1, 25)
            };

            var act = async () => await _workOrderService.CreateWorkOrderAsync(command);
            await act.Should().ThrowAsync<ArgumentException>()
                .WithMessage("Routing not found for item: PROD-003");
        }
    }

    public class 作業指示のステータス管理 : WorkOrderServiceTests
    {
        public 作業指示のステータス管理(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 作業指示を開始できる()
        {
            // Arrange: マスタデータを準備
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "PROD-004",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品D",
                ItemCategory = ItemCategory.Product
            });

            await _locationRepository.SaveAsync(new Location
            {
                LocationCode = "LINE-004",
                LocationName = "製造ライン4",
                LocationType = LocationType.Manufacturing
            });

            await _processRepository.SaveAsync(new Process
            {
                ProcessCode = "PAINT",
                ProcessName = "塗装"
            });

            await _routingRepository.SaveAsync(new Routing
            {
                ItemCode = "PROD-004",
                Sequence = 1,
                ProcessCode = "PAINT"
            });

            await _orderRepository.SaveAsync(new Order
            {
                OrderNumber = "MO-2025-004",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-004",
                StartDate = new DateOnly(2025, 1, 21),
                DueDate = new DateOnly(2025, 1, 25),
                PlanQuantity = 20m,
                LocationCode = "LINE-004",
                Status = PlanStatus.Confirmed
            });

            var createCommand = new WorkOrderCreateCommand
            {
                OrderNumber = "MO-2025-004",
                WorkOrderDate = new DateOnly(2025, 1, 20),
                LocationCode = "LINE-004",
                PlannedStartDate = new DateOnly(2025, 1, 21),
                PlannedEndDate = new DateOnly(2025, 1, 25)
            };
            var workOrder = await _workOrderService.CreateWorkOrderAsync(createCommand);

            // Act: 作業開始
            var startedWorkOrder = await _workOrderService.StartWorkAsync(workOrder.WorkOrderNumber);

            // Assert
            startedWorkOrder.Status.Should().Be(WorkOrderStatus.InProgress);
            startedWorkOrder.ActualStartDate.Should().NotBeNull();
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 作業指示を完了できる()
        {
            // Arrange
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "PROD-005",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品E",
                ItemCategory = ItemCategory.Product
            });

            await _locationRepository.SaveAsync(new Location
            {
                LocationCode = "LINE-005",
                LocationName = "製造ライン5",
                LocationType = LocationType.Manufacturing
            });

            await _processRepository.SaveAsync(new Process
            {
                ProcessCode = "PACK",
                ProcessName = "梱包"
            });

            await _routingRepository.SaveAsync(new Routing
            {
                ItemCode = "PROD-005",
                Sequence = 1,
                ProcessCode = "PACK"
            });

            await _orderRepository.SaveAsync(new Order
            {
                OrderNumber = "MO-2025-005",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-005",
                StartDate = new DateOnly(2025, 1, 21),
                DueDate = new DateOnly(2025, 1, 25),
                PlanQuantity = 15m,
                LocationCode = "LINE-005",
                Status = PlanStatus.Confirmed
            });

            var createCommand = new WorkOrderCreateCommand
            {
                OrderNumber = "MO-2025-005",
                WorkOrderDate = new DateOnly(2025, 1, 20),
                LocationCode = "LINE-005",
                PlannedStartDate = new DateOnly(2025, 1, 21),
                PlannedEndDate = new DateOnly(2025, 1, 25)
            };
            var workOrder = await _workOrderService.CreateWorkOrderAsync(createCommand);
            await _workOrderService.StartWorkAsync(workOrder.WorkOrderNumber);

            // Act: 作業完了
            var completedWorkOrder = await _workOrderService.CompleteWorkAsync(workOrder.WorkOrderNumber);

            // Assert
            completedWorkOrder.Status.Should().Be(WorkOrderStatus.Completed);
            completedWorkOrder.CompletedFlag.Should().BeTrue();
            completedWorkOrder.ActualEndDate.Should().NotBeNull();
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 未着手以外の作業指示は開始できない()
        {
            // Arrange
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "PROD-006",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品F",
                ItemCategory = ItemCategory.Product
            });

            await _locationRepository.SaveAsync(new Location
            {
                LocationCode = "LINE-006",
                LocationName = "製造ライン6",
                LocationType = LocationType.Manufacturing
            });

            await _processRepository.SaveAsync(new Process
            {
                ProcessCode = "TEST",
                ProcessName = "テスト"
            });

            await _routingRepository.SaveAsync(new Routing
            {
                ItemCode = "PROD-006",
                Sequence = 1,
                ProcessCode = "TEST"
            });

            await _orderRepository.SaveAsync(new Order
            {
                OrderNumber = "MO-2025-006",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-006",
                StartDate = new DateOnly(2025, 1, 21),
                DueDate = new DateOnly(2025, 1, 25),
                PlanQuantity = 10m,
                LocationCode = "LINE-006",
                Status = PlanStatus.Confirmed
            });

            var createCommand = new WorkOrderCreateCommand
            {
                OrderNumber = "MO-2025-006",
                WorkOrderDate = new DateOnly(2025, 1, 20),
                LocationCode = "LINE-006",
                PlannedStartDate = new DateOnly(2025, 1, 21),
                PlannedEndDate = new DateOnly(2025, 1, 25)
            };
            var workOrder = await _workOrderService.CreateWorkOrderAsync(createCommand);
            await _workOrderService.StartWorkAsync(workOrder.WorkOrderNumber);

            // Act & Assert: 既に作業中の作業指示を開始しようとするとエラー
            var act = async () => await _workOrderService.StartWorkAsync(workOrder.WorkOrderNumber);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Only NOT_STARTED work orders can be started");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 作業中以外の作業指示は完了できない()
        {
            // Arrange
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "PROD-007",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品G",
                ItemCategory = ItemCategory.Product
            });

            await _locationRepository.SaveAsync(new Location
            {
                LocationCode = "LINE-007",
                LocationName = "製造ライン7",
                LocationType = LocationType.Manufacturing
            });

            await _processRepository.SaveAsync(new Process
            {
                ProcessCode = "FINAL",
                ProcessName = "最終工程"
            });

            await _routingRepository.SaveAsync(new Routing
            {
                ItemCode = "PROD-007",
                Sequence = 1,
                ProcessCode = "FINAL"
            });

            await _orderRepository.SaveAsync(new Order
            {
                OrderNumber = "MO-2025-007",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-007",
                StartDate = new DateOnly(2025, 1, 21),
                DueDate = new DateOnly(2025, 1, 25),
                PlanQuantity = 5m,
                LocationCode = "LINE-007",
                Status = PlanStatus.Confirmed
            });

            var createCommand = new WorkOrderCreateCommand
            {
                OrderNumber = "MO-2025-007",
                WorkOrderDate = new DateOnly(2025, 1, 20),
                LocationCode = "LINE-007",
                PlannedStartDate = new DateOnly(2025, 1, 21),
                PlannedEndDate = new DateOnly(2025, 1, 25)
            };
            var workOrder = await _workOrderService.CreateWorkOrderAsync(createCommand);
            // 開始しない

            // Act & Assert: 未着手の作業指示を完了しようとするとエラー
            var act = async () => await _workOrderService.CompleteWorkAsync(workOrder.WorkOrderNumber);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Only IN_PROGRESS work orders can be completed");
        }
    }
}
