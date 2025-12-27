using FluentAssertions;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Location;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Domain.Models.Process;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Services;

/// <summary>
/// 完成実績サービステスト
/// </summary>
[Collection("Database")]
public class CompletionResultServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly CompletionResultService _completionResultService;
    private readonly WorkOrderService _workOrderService;
    private readonly ICompletionResultRepository _completionResultRepository;
    private readonly ICompletionInspectionResultRepository _completionInspectionResultRepository;
    private readonly IWorkOrderRepository _workOrderRepository;
    private readonly IWorkOrderDetailRepository _workOrderDetailRepository;
    private readonly IOrderRepository _orderRepository;
    private readonly IRoutingRepository _routingRepository;
    private readonly IProcessRepository _processRepository;
    private readonly ILocationRepository _locationRepository;
    private readonly IItemRepository _itemRepository;
    private readonly IDefectRepository _defectRepository;
    private readonly IAllocationRepository _allocationRepository;
    private readonly IRequirementRepository _requirementRepository;

    public CompletionResultServiceTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _completionResultRepository = new CompletionResultRepository(fixture.ConnectionString);
        _completionInspectionResultRepository = new CompletionInspectionResultRepository(fixture.ConnectionString);
        _workOrderRepository = new WorkOrderRepository(fixture.ConnectionString);
        _workOrderDetailRepository = new WorkOrderDetailRepository(fixture.ConnectionString);
        _orderRepository = new OrderRepository(fixture.ConnectionString);
        _routingRepository = new RoutingRepository(fixture.ConnectionString);
        _processRepository = new ProcessRepository(fixture.ConnectionString);
        _locationRepository = new LocationRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _defectRepository = new DefectRepository(fixture.ConnectionString);
        _allocationRepository = new AllocationRepository(fixture.ConnectionString);
        _requirementRepository = new RequirementRepository(fixture.ConnectionString);

        _completionResultService = new CompletionResultService(
            _completionResultRepository,
            _completionInspectionResultRepository,
            _workOrderRepository);

        _workOrderService = new WorkOrderService(
            _workOrderRepository,
            _workOrderDetailRepository,
            _orderRepository,
            _routingRepository);

        // FK制約の順序に従って削除
        _completionInspectionResultRepository.DeleteAllAsync().Wait();
        _completionResultRepository.DeleteAllAsync().Wait();
        _workOrderDetailRepository.DeleteAllAsync().Wait();
        _workOrderRepository.DeleteAllAsync().Wait();
        _allocationRepository.DeleteAllAsync().Wait();
        _requirementRepository.DeleteAllAsync().Wait();
        _routingRepository.DeleteAllAsync().Wait();
        _processRepository.DeleteAllAsync().Wait();
        _orderRepository.DeleteAllAsync().Wait();
        _defectRepository.DeleteAllAsync().Wait();
        _locationRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    private async Task<WorkOrder> CreateTestWorkOrderAsync()
    {
        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "PROD-CR-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "完成実績テスト製品",
            ItemCategory = ItemCategory.Product
        });

        await _locationRepository.SaveAsync(new Location
        {
            LocationCode = "LINE-CR-001",
            LocationName = "完成実績テストライン",
            LocationType = LocationType.Manufacturing
        });

        await _processRepository.SaveAsync(new Process
        {
            ProcessCode = "ASSEMBLE",
            ProcessName = "組立工程"
        });

        await _routingRepository.SaveAsync(new Routing
        {
            ItemCode = "PROD-CR-001",
            Sequence = 1,
            ProcessCode = "ASSEMBLE"
        });

        await _orderRepository.SaveAsync(new Order
        {
            OrderNumber = "MO-CR-001",
            OrderType = OrderType.Manufacturing,
            ItemCode = "PROD-CR-001",
            StartDate = new DateOnly(2025, 1, 21),
            DueDate = new DateOnly(2025, 1, 25),
            PlanQuantity = 100m,
            LocationCode = "LINE-CR-001",
            Status = PlanStatus.Confirmed
        });

        var createCommand = new WorkOrderCreateCommand
        {
            OrderNumber = "MO-CR-001",
            WorkOrderDate = new DateOnly(2025, 1, 20),
            LocationCode = "LINE-CR-001",
            PlannedStartDate = new DateOnly(2025, 1, 21),
            PlannedEndDate = new DateOnly(2025, 1, 25)
        };
        var workOrder = await _workOrderService.CreateWorkOrderAsync(createCommand);

        // 作業開始
        await _workOrderService.StartWorkAsync(workOrder.WorkOrderNumber);
        return await _workOrderRepository.FindByWorkOrderNumberAsync(workOrder.WorkOrderNumber)
            ?? throw new InvalidOperationException("Work order not found");
    }

    public class 完成実績の報告 : CompletionResultServiceTests
    {
        public 完成実績の報告(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 完成実績を報告できる()
        {
            // Arrange
            var workOrder = await CreateTestWorkOrderAsync();

            // Act
            var command = new CompletionResultCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                CompletionDate = new DateOnly(2025, 1, 22),
                CompletedQuantity = 50m,
                GoodQuantity = 48m,
                DefectQuantity = 2m,
                Remarks = "初回完成報告"
            };

            var completionResult = await _completionResultService.ReportCompletionAsync(command);

            // Assert
            completionResult.Should().NotBeNull();
            completionResult.CompletionResultNumber.Should().StartWith("CR-202501-");
            completionResult.ItemCode.Should().Be("PROD-CR-001");
            completionResult.CompletedQuantity.Should().Be(50m);
            completionResult.GoodQuantity.Should().Be(48m);
            completionResult.DefectQuantity.Should().Be(2m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 完成実績報告で作業指示の累計が更新される()
        {
            // Arrange
            var workOrder = await CreateTestWorkOrderAsync();

            // Act: 1回目の完成報告
            var command1 = new CompletionResultCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                CompletionDate = new DateOnly(2025, 1, 22),
                CompletedQuantity = 30m,
                GoodQuantity = 29m,
                DefectQuantity = 1m
            };
            await _completionResultService.ReportCompletionAsync(command1);

            // Act: 2回目の完成報告
            var command2 = new CompletionResultCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                CompletionDate = new DateOnly(2025, 1, 23),
                CompletedQuantity = 40m,
                GoodQuantity = 38m,
                DefectQuantity = 2m
            };
            await _completionResultService.ReportCompletionAsync(command2);

            // Assert: 累計を確認
            var updatedWorkOrder = await _workOrderRepository.FindByWorkOrderNumberAsync(workOrder.WorkOrderNumber);
            updatedWorkOrder.Should().NotBeNull();
            updatedWorkOrder!.CompletedQuantity.Should().Be(70m);
            updatedWorkOrder.TotalGoodQuantity.Should().Be(67m);
            updatedWorkOrder.TotalDefectQuantity.Should().Be(3m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 作業中以外の作業指示には完成報告できない()
        {
            // Arrange: 作業指示を作成（開始していない）
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "PROD-CR-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "未開始テスト製品",
                ItemCategory = ItemCategory.Product
            });

            await _locationRepository.SaveAsync(new Location
            {
                LocationCode = "LINE-CR-002",
                LocationName = "未開始テストライン",
                LocationType = LocationType.Manufacturing
            });

            await _processRepository.SaveAsync(new Process
            {
                ProcessCode = "PROCESS2",
                ProcessName = "工程2"
            });

            await _routingRepository.SaveAsync(new Routing
            {
                ItemCode = "PROD-CR-002",
                Sequence = 1,
                ProcessCode = "PROCESS2"
            });

            await _orderRepository.SaveAsync(new Order
            {
                OrderNumber = "MO-CR-002",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-CR-002",
                StartDate = new DateOnly(2025, 1, 21),
                DueDate = new DateOnly(2025, 1, 25),
                PlanQuantity = 50m,
                LocationCode = "LINE-CR-002",
                Status = PlanStatus.Confirmed
            });

            var createCommand = new WorkOrderCreateCommand
            {
                OrderNumber = "MO-CR-002",
                WorkOrderDate = new DateOnly(2025, 1, 20),
                LocationCode = "LINE-CR-002",
                PlannedStartDate = new DateOnly(2025, 1, 21),
                PlannedEndDate = new DateOnly(2025, 1, 25)
            };
            var workOrder = await _workOrderService.CreateWorkOrderAsync(createCommand);

            // 開始しない

            // Act & Assert
            var command = new CompletionResultCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                CompletionDate = new DateOnly(2025, 1, 22),
                CompletedQuantity = 10m,
                GoodQuantity = 10m,
                DefectQuantity = 0m
            };

            var act = async () => await _completionResultService.ReportCompletionAsync(command);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Only IN_PROGRESS work orders can report completion");
        }
    }

    public class 完成検査結果の報告 : CompletionResultServiceTests
    {
        public 完成検査結果の報告(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 欠点情報付きで完成実績を報告できる()
        {
            // Arrange
            var workOrder = await CreateTestWorkOrderAsync();

            await _defectRepository.SaveAsync(new Defect
            {
                DefectCode = "SCRATCH",
                DefectName = "キズ",
                DefectCategory = "外観",
                DisplayOrder = 1
            });

            await _defectRepository.SaveAsync(new Defect
            {
                DefectCode = "DENT",
                DefectName = "ヘコミ",
                DefectCategory = "外観",
                DisplayOrder = 2
            });

            // Act
            var command = new CompletionResultCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                CompletionDate = new DateOnly(2025, 1, 22),
                CompletedQuantity = 100m,
                GoodQuantity = 95m,
                DefectQuantity = 5m,
                InspectionResults =
                [
                    new CompletionInspectionResultCommand { DefectCode = "SCRATCH", Quantity = 3m },
                    new CompletionInspectionResultCommand { DefectCode = "DENT", Quantity = 2m }
                ]
            };

            var completionResult = await _completionResultService.ReportCompletionAsync(command);

            // Assert
            completionResult.InspectionResults.Should().HaveCount(2);
            completionResult.InspectionResults.Should().Contain(ir => ir.DefectCode == "SCRATCH" && ir.Quantity == 3m);
            completionResult.InspectionResults.Should().Contain(ir => ir.DefectCode == "DENT" && ir.Quantity == 2m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 検査結果を含む完成実績を検索できる()
        {
            // Arrange
            var workOrder = await CreateTestWorkOrderAsync();

            await _defectRepository.SaveAsync(new Defect
            {
                DefectCode = "CRACK",
                DefectName = "ひび割れ",
                DefectCategory = "構造",
                DisplayOrder = 1
            });

            var createCommand = new CompletionResultCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                CompletionDate = new DateOnly(2025, 1, 22),
                CompletedQuantity = 50m,
                GoodQuantity = 49m,
                DefectQuantity = 1m,
                InspectionResults =
                [
                    new CompletionInspectionResultCommand { DefectCode = "CRACK", Quantity = 1m }
                ]
            };

            var created = await _completionResultService.ReportCompletionAsync(createCommand);

            // Act
            var found = await _completionResultService.FindByCompletionResultNumberAsync(created.CompletionResultNumber);

            // Assert
            found.Should().NotBeNull();
            found!.InspectionResults.Should().HaveCount(1);
            found.InspectionResults[0].DefectCode.Should().Be("CRACK");
        }
    }
}
