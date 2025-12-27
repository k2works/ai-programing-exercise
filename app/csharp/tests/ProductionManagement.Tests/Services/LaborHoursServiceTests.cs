using FluentAssertions;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Location;
using ProductionManagement.Domain.Models.Master;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Domain.Models.Process;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Services;

/// <summary>
/// 工数実績サービステスト
/// </summary>
[Collection("Database")]
public class LaborHoursServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly LaborHoursService _laborHoursService;
    private readonly WorkOrderService _workOrderService;
    private readonly ILaborHoursRepository _laborHoursRepository;
    private readonly IWorkOrderRepository _workOrderRepository;
    private readonly IWorkOrderDetailRepository _workOrderDetailRepository;
    private readonly IOrderRepository _orderRepository;
    private readonly IRoutingRepository _routingRepository;
    private readonly IProcessRepository _processRepository;
    private readonly ILocationRepository _locationRepository;
    private readonly IItemRepository _itemRepository;
    private readonly IDepartmentRepository _departmentRepository;
    private readonly IEmployeeRepository _employeeRepository;
    private readonly ICompletionInspectionResultRepository _completionInspectionResultRepository;
    private readonly ICompletionResultRepository _completionResultRepository;
    private readonly IAllocationRepository _allocationRepository;
    private readonly IRequirementRepository _requirementRepository;

    public LaborHoursServiceTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _laborHoursRepository = new LaborHoursRepository(fixture.ConnectionString);
        _workOrderRepository = new WorkOrderRepository(fixture.ConnectionString);
        _workOrderDetailRepository = new WorkOrderDetailRepository(fixture.ConnectionString);
        _orderRepository = new OrderRepository(fixture.ConnectionString);
        _routingRepository = new RoutingRepository(fixture.ConnectionString);
        _processRepository = new ProcessRepository(fixture.ConnectionString);
        _locationRepository = new LocationRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _departmentRepository = new DepartmentRepository(fixture.ConnectionString);
        _employeeRepository = new EmployeeRepository(fixture.ConnectionString);
        _completionInspectionResultRepository = new CompletionInspectionResultRepository(fixture.ConnectionString);
        _completionResultRepository = new CompletionResultRepository(fixture.ConnectionString);
        _allocationRepository = new AllocationRepository(fixture.ConnectionString);
        _requirementRepository = new RequirementRepository(fixture.ConnectionString);

        _laborHoursService = new LaborHoursService(
            _laborHoursRepository,
            _workOrderDetailRepository,
            _workOrderRepository,
            _processRepository);

        _workOrderService = new WorkOrderService(
            _workOrderRepository,
            _workOrderDetailRepository,
            _orderRepository,
            _routingRepository);

        // FK制約の順序に従って削除
        _laborHoursRepository.DeleteAllAsync().Wait();
        _employeeRepository.DeleteAllAsync().Wait();
        _departmentRepository.DeleteAllAsync().Wait();
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

    private async Task<WorkOrder> CreateTestWorkOrderAsync()
    {
        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "PROD-LH-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "工数実績テスト製品",
            ItemCategory = ItemCategory.Product
        });

        await _locationRepository.SaveAsync(new Location
        {
            LocationCode = "LINE-LH-001",
            LocationName = "工数実績テストライン",
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
            ItemCode = "PROD-LH-001",
            Sequence = 1,
            ProcessCode = "PRESS"
        });

        await _routingRepository.SaveAsync(new Routing
        {
            ItemCode = "PROD-LH-001",
            Sequence = 2,
            ProcessCode = "ASSEMBLY"
        });

        await _orderRepository.SaveAsync(new Order
        {
            OrderNumber = "MO-LH-001",
            OrderType = OrderType.Manufacturing,
            ItemCode = "PROD-LH-001",
            StartDate = new DateOnly(2025, 1, 21),
            DueDate = new DateOnly(2025, 1, 25),
            PlanQuantity = 100m,
            LocationCode = "LINE-LH-001",
            Status = PlanStatus.Confirmed
        });

        await _departmentRepository.SaveAsync(new Department
        {
            DepartmentCode = "DEPT-001",
            DepartmentName = "製造部"
        });

        await _employeeRepository.SaveAsync(new Employee
        {
            EmployeeCode = "EMP-001",
            EmployeeName = "山田太郎",
            DepartmentCode = "DEPT-001"
        });

        await _employeeRepository.SaveAsync(new Employee
        {
            EmployeeCode = "EMP-002",
            EmployeeName = "鈴木花子",
            DepartmentCode = "DEPT-001"
        });

        var createCommand = new WorkOrderCreateCommand
        {
            OrderNumber = "MO-LH-001",
            WorkOrderDate = new DateOnly(2025, 1, 20),
            LocationCode = "LINE-LH-001",
            PlannedStartDate = new DateOnly(2025, 1, 21),
            PlannedEndDate = new DateOnly(2025, 1, 25)
        };
        var workOrder = await _workOrderService.CreateWorkOrderAsync(createCommand);

        // 作業開始
        await _workOrderService.StartWorkAsync(workOrder.WorkOrderNumber);
        return await _workOrderRepository.FindByWorkOrderNumberAsync(workOrder.WorkOrderNumber)
            ?? throw new InvalidOperationException("Work order not found");
    }

    public class 工数実績の報告 : LaborHoursServiceTests
    {
        public 工数実績の報告(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 工数実績を報告できる()
        {
            // Arrange
            var workOrder = await CreateTestWorkOrderAsync();

            // Act
            var command = new LaborHoursCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                Sequence = 1,
                WorkDate = new DateOnly(2025, 1, 22),
                EmployeeCode = "EMP-001",
                DepartmentCode = "DEPT-001",
                Hours = 2.5m,
                Remarks = "プレス加工作業"
            };

            var laborHours = await _laborHoursService.ReportLaborHoursAsync(command);

            // Assert
            laborHours.Should().NotBeNull();
            laborHours.LaborHoursNumber.Should().StartWith("LH-202501-");
            laborHours.ItemCode.Should().Be("PROD-LH-001");
            laborHours.ProcessCode.Should().Be("PRESS");
            laborHours.Hours.Should().Be(2.5m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 複数の工数実績を報告できる()
        {
            // Arrange
            var workOrder = await CreateTestWorkOrderAsync();

            // Act: 1回目の報告
            var command1 = new LaborHoursCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                Sequence = 1,
                WorkDate = new DateOnly(2025, 1, 22),
                EmployeeCode = "EMP-001",
                DepartmentCode = "DEPT-001",
                Hours = 2.0m
            };
            await _laborHoursService.ReportLaborHoursAsync(command1);

            // Act: 2回目の報告（別の担当者）
            var command2 = new LaborHoursCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                Sequence = 1,
                WorkDate = new DateOnly(2025, 1, 22),
                EmployeeCode = "EMP-002",
                DepartmentCode = "DEPT-001",
                Hours = 1.5m
            };
            await _laborHoursService.ReportLaborHoursAsync(command2);

            // Assert: 工順別の合計を確認
            var totalHours = await _laborHoursService.GetTotalHoursBySequenceAsync(workOrder.WorkOrderNumber, 1);
            totalHours.Should().Be(3.5m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 作業中以外の作業指示には工数報告できない()
        {
            // Arrange: 作業指示を作成（開始していない）
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "PROD-LH-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "未開始テスト製品",
                ItemCategory = ItemCategory.Product
            });

            await _locationRepository.SaveAsync(new Location
            {
                LocationCode = "LINE-LH-002",
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
                ItemCode = "PROD-LH-002",
                Sequence = 1,
                ProcessCode = "PROCESS2"
            });

            await _orderRepository.SaveAsync(new Order
            {
                OrderNumber = "MO-LH-002",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-LH-002",
                StartDate = new DateOnly(2025, 1, 21),
                DueDate = new DateOnly(2025, 1, 25),
                PlanQuantity = 50m,
                LocationCode = "LINE-LH-002",
                Status = PlanStatus.Confirmed
            });

            await _departmentRepository.SaveAsync(new Department
            {
                DepartmentCode = "DEPT-002",
                DepartmentName = "製造部2"
            });

            await _employeeRepository.SaveAsync(new Employee
            {
                EmployeeCode = "EMP-003",
                EmployeeName = "佐藤次郎",
                DepartmentCode = "DEPT-002"
            });

            var createCommand = new WorkOrderCreateCommand
            {
                OrderNumber = "MO-LH-002",
                WorkOrderDate = new DateOnly(2025, 1, 20),
                LocationCode = "LINE-LH-002",
                PlannedStartDate = new DateOnly(2025, 1, 21),
                PlannedEndDate = new DateOnly(2025, 1, 25)
            };
            var workOrder = await _workOrderService.CreateWorkOrderAsync(createCommand);

            // 開始しない

            // Act & Assert
            var command = new LaborHoursCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                Sequence = 1,
                WorkDate = new DateOnly(2025, 1, 22),
                EmployeeCode = "EMP-003",
                DepartmentCode = "DEPT-002",
                Hours = 1.0m
            };

            var act = async () => await _laborHoursService.ReportLaborHoursAsync(command);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Only IN_PROGRESS work orders can report labor hours");
        }
    }

    public class 工数サマリの取得 : LaborHoursServiceTests
    {
        public 工数サマリの取得(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 工程別の工数サマリを取得できる()
        {
            // Arrange
            var workOrder = await CreateTestWorkOrderAsync();

            // 各工程に工数を報告
            await _laborHoursService.ReportLaborHoursAsync(new LaborHoursCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                Sequence = 1,
                WorkDate = new DateOnly(2025, 1, 22),
                EmployeeCode = "EMP-001",
                DepartmentCode = "DEPT-001",
                Hours = 3.0m
            });

            await _laborHoursService.ReportLaborHoursAsync(new LaborHoursCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                Sequence = 2,
                WorkDate = new DateOnly(2025, 1, 23),
                EmployeeCode = "EMP-001",
                DepartmentCode = "DEPT-001",
                Hours = 2.0m
            });

            // Act
            var summary = await _laborHoursService.GetSummaryAsync(workOrder.WorkOrderNumber);

            // Assert
            summary.TotalHours.Should().Be(5.0m);
            summary.ProcessHours.Should().HaveCount(2);
            summary.ProcessHours.Should().Contain(p => p.ProcessCode == "PRESS" && p.Hours == 3.0m);
            summary.ProcessHours.Should().Contain(p => p.ProcessCode == "ASSEMBLY" && p.Hours == 2.0m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 担当者別の工数を取得できる()
        {
            // Arrange
            var workOrder = await CreateTestWorkOrderAsync();

            await _laborHoursService.ReportLaborHoursAsync(new LaborHoursCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                Sequence = 1,
                WorkDate = new DateOnly(2025, 1, 22),
                EmployeeCode = "EMP-001",
                DepartmentCode = "DEPT-001",
                Hours = 4.0m
            });

            await _laborHoursService.ReportLaborHoursAsync(new LaborHoursCommand
            {
                WorkOrderNumber = workOrder.WorkOrderNumber,
                Sequence = 1,
                WorkDate = new DateOnly(2025, 1, 23),
                EmployeeCode = "EMP-001",
                DepartmentCode = "DEPT-001",
                Hours = 3.5m
            });

            // Act
            var totalHours = await _laborHoursService.GetTotalHoursByEmployeeAsync(
                "EMP-001",
                new DateOnly(2025, 1, 1),
                new DateOnly(2025, 1, 31));

            // Assert
            totalHours.Should().Be(7.5m);
        }
    }
}
