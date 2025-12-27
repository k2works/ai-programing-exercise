using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Models.Bom;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Services;

/// <summary>
/// 所要量展開（MRP）サービステスト
/// </summary>
[Collection("Database")]
public class MrpServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly MrpService _mrpService;
    private readonly IItemRepository _itemRepository;
    private readonly IBomRepository _bomRepository;
    private readonly IOrderRepository _orderRepository;
    private readonly IRequirementRepository _requirementRepository;
    private readonly IAllocationRepository _allocationRepository;
    private readonly IWorkOrderDetailRepository _workOrderDetailRepository;
    private readonly IWorkOrderRepository _workOrderRepository;
    private readonly ICompletionInspectionResultRepository _completionInspectionResultRepository;
    private readonly ICompletionResultRepository _completionResultRepository;
    private readonly ILaborHoursRepository _laborHoursRepository;
    private readonly IEmployeeRepository _employeeRepository;
    private readonly IDepartmentRepository _departmentRepository;
    private readonly IIssueRepository _issueRepository;

    public MrpServiceTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _bomRepository = new BomRepository(fixture.ConnectionString);
        _orderRepository = new OrderRepository(fixture.ConnectionString);
        _requirementRepository = new RequirementRepository(fixture.ConnectionString);
        _allocationRepository = new AllocationRepository(fixture.ConnectionString);
        _workOrderDetailRepository = new WorkOrderDetailRepository(fixture.ConnectionString);
        _workOrderRepository = new WorkOrderRepository(fixture.ConnectionString);
        _completionInspectionResultRepository = new CompletionInspectionResultRepository(fixture.ConnectionString);
        _completionResultRepository = new CompletionResultRepository(fixture.ConnectionString);
        _laborHoursRepository = new LaborHoursRepository(fixture.ConnectionString);
        _employeeRepository = new EmployeeRepository(fixture.ConnectionString);
        _departmentRepository = new DepartmentRepository(fixture.ConnectionString);
        _issueRepository = new IssueRepository(fixture.ConnectionString);

        _mrpService = new MrpService(
            _itemRepository,
            _bomRepository,
            _orderRepository,
            _requirementRepository,
            _allocationRepository
        );

        CleanupAsync().Wait();
    }

    private async Task CleanupAsync()
    {
        // FK制約の順序に従って削除（払出 → 工数実績 → 完成実績 → 作業指示 → Order）
        await _issueRepository.DeleteAllDetailsAsync();
        await _issueRepository.DeleteAllAsync();
        await _laborHoursRepository.DeleteAllAsync();
        await _employeeRepository.DeleteAllAsync();
        await _departmentRepository.DeleteAllAsync();
        await _completionInspectionResultRepository.DeleteAllAsync();
        await _completionResultRepository.DeleteAllAsync();
        await _workOrderDetailRepository.DeleteAllAsync();
        await _workOrderRepository.DeleteAllAsync();
        await _allocationRepository.DeleteAllAsync();
        await _requirementRepository.DeleteAllAsync();
        await _orderRepository.DeleteAllAsync();
        await _bomRepository.DeleteAllAsync();
        await _itemRepository.DeleteAllAsync();
    }

    public class SimpleRequirementCalculation : MrpServiceTests
    {
        public SimpleRequirementCalculation(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 製品オーダから子部品の所要量を計算できる()
        {
            // Arrange: 製品と部品を登録
            var product = new Item
            {
                ItemCode = "PROD-MRP-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品A",
                ItemCategory = ItemCategory.Product,
                LeadTime = 5
            };
            await _itemRepository.SaveAsync(product);

            var part = new Item
            {
                ItemCode = "PART-MRP-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "部品B",
                ItemCategory = ItemCategory.Part,
                LeadTime = 3
            };
            await _itemRepository.SaveAsync(part);

            // BOM登録（製品1個に部品2個必要）
            var bom = new Bom
            {
                ParentItemCode = "PROD-MRP-001",
                ChildItemCode = "PART-MRP-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                BaseQuantity = 1m,
                RequiredQuantity = 2m,
                DefectRate = 0m
            };
            await _bomRepository.SaveAsync(bom);

            // 製品オーダを作成
            var productOrder = new Order
            {
                OrderNumber = "MO-MRP-001",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-MRP-001",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Confirmed
            };
            await _orderRepository.SaveAsync(productOrder);

            // Act: MRP実行
            var requirements = await _mrpService.ExplodeRequirementsAsync(productOrder.Id);

            // Assert: 部品の所要量が200個（100 × 2）になる
            requirements.Should().HaveCount(1);
            requirements[0].ItemCode.Should().Be("PART-MRP-001");
            requirements[0].RequiredQuantity.Should().Be(200m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 不良率を考慮して所要量を計算できる()
        {
            // Arrange
            var product = new Item
            {
                ItemCode = "PROD-MRP-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品B",
                ItemCategory = ItemCategory.Product,
                LeadTime = 5
            };
            await _itemRepository.SaveAsync(product);

            var part = new Item
            {
                ItemCode = "PART-MRP-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "部品C",
                ItemCategory = ItemCategory.Part,
                LeadTime = 3
            };
            await _itemRepository.SaveAsync(part);

            // BOM登録（製品1個に部品1個必要、不良率10%）
            var bom = new Bom
            {
                ParentItemCode = "PROD-MRP-002",
                ChildItemCode = "PART-MRP-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                BaseQuantity = 1m,
                RequiredQuantity = 1m,
                DefectRate = 10m
            };
            await _bomRepository.SaveAsync(bom);

            var productOrder = new Order
            {
                OrderNumber = "MO-MRP-002",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-MRP-002",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Confirmed
            };
            await _orderRepository.SaveAsync(productOrder);

            // Act
            var requirements = await _mrpService.ExplodeRequirementsAsync(productOrder.Id);

            // Assert: 100 × 1 × 1.10 = 110
            requirements.Should().HaveCount(1);
            requirements[0].RequiredQuantity.Should().Be(110m);
        }
    }

    public class InventoryAllocation : MrpServiceTests
    {
        public InventoryAllocation(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 在庫がある場合は引当される()
        {
            // Arrange
            var part = new Item
            {
                ItemCode = "PART-ALLOC-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "部品D",
                ItemCategory = ItemCategory.Part
            };
            await _itemRepository.SaveAsync(part);

            var order = new Order
            {
                OrderNumber = "MO-ALLOC-001",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PART-ALLOC-001",
                StartDate = new DateOnly(2025, 1, 10),
                DueDate = new DateOnly(2025, 1, 15),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Confirmed
            };
            await _orderRepository.SaveAsync(order);

            var requirement = new Requirement
            {
                RequirementNumber = "REQ-ALLOC-001",
                OrderId = order.Id,
                ItemCode = "PART-ALLOC-001",
                DueDate = new DateOnly(2025, 1, 15),
                RequiredQuantity = 100m,
                AllocatedQuantity = 0m,
                ShortageQuantity = 100m,
                LocationCode = "WH-001"
            };
            await _requirementRepository.SaveAsync(requirement);

            decimal currentInventory = 50m;

            // Act: 在庫引当を実行
            var allocation = await _mrpService.AllocateFromInventoryAsync(
                requirement.Id, currentInventory);

            // Assert: 50個が引当され、50個が不足
            allocation.AllocatedQuantity.Should().Be(50m);

            var updated = await _requirementRepository.FindByIdAsync(requirement.Id);
            updated.Should().NotBeNull();
            updated!.AllocatedQuantity.Should().Be(50m);
            updated.ShortageQuantity.Should().Be(50m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 在庫が十分にある場合は全数引当される()
        {
            // Arrange
            var part = new Item
            {
                ItemCode = "PART-ALLOC-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "部品E",
                ItemCategory = ItemCategory.Part
            };
            await _itemRepository.SaveAsync(part);

            var order = new Order
            {
                OrderNumber = "MO-ALLOC-002",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PART-ALLOC-002",
                StartDate = new DateOnly(2025, 1, 10),
                DueDate = new DateOnly(2025, 1, 15),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Confirmed
            };
            await _orderRepository.SaveAsync(order);

            var requirement = new Requirement
            {
                RequirementNumber = "REQ-ALLOC-002",
                OrderId = order.Id,
                ItemCode = "PART-ALLOC-002",
                DueDate = new DateOnly(2025, 1, 15),
                RequiredQuantity = 100m,
                AllocatedQuantity = 0m,
                ShortageQuantity = 100m,
                LocationCode = "WH-001"
            };
            await _requirementRepository.SaveAsync(requirement);

            decimal currentInventory = 150m;

            // Act
            var allocation = await _mrpService.AllocateFromInventoryAsync(
                requirement.Id, currentInventory);

            // Assert: 100個が引当され、不足は0
            allocation.AllocatedQuantity.Should().Be(100m);

            var updated = await _requirementRepository.FindByIdAsync(requirement.Id);
            updated.Should().NotBeNull();
            updated!.AllocatedQuantity.Should().Be(100m);
            updated.ShortageQuantity.Should().Be(0m);
        }
    }

    public class LotSizeCalculation : MrpServiceTests
    {
        public LotSizeCalculation(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public void 最小ロットサイズを考慮してオーダ数量を計算する()
        {
            // Act: 必要数量75個に対するオーダ数量を計算
            var orderQuantity = _mrpService.CalculateOrderQuantity(
                75m,      // 必要数量
                100m,     // 最小ロット数
                50m,      // 刻みロット数
                null      // 最大ロット数
            );

            // Assert: 最小ロット100個になる
            orderQuantity.Should().Be(100m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public void 刻みロットサイズを考慮してオーダ数量を計算する()
        {
            // Act: 必要数量130個に対するオーダ数量を計算
            var orderQuantity = _mrpService.CalculateOrderQuantity(
                130m,     // 必要数量
                100m,     // 最小ロット数
                50m,      // 刻みロット数
                null      // 最大ロット数
            );

            // Assert: 100 + 50 = 150個になる
            orderQuantity.Should().Be(150m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public void 最大ロットサイズを超えないようにする()
        {
            // Act: 必要数量300個に対するオーダ数量を計算
            var orderQuantity = _mrpService.CalculateOrderQuantity(
                300m,     // 必要数量
                100m,     // 最小ロット数
                50m,      // 刻みロット数
                250m      // 最大ロット数
            );

            // Assert: 最大250個になる
            orderQuantity.Should().Be(250m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public void デフォルト値でオーダ数量を計算する()
        {
            // Act: ロットサイズ未設定の場合
            var orderQuantity = _mrpService.CalculateOrderQuantity(
                75m,     // 必要数量
                null,    // 最小ロット数（デフォルト1）
                null,    // 刻みロット数（デフォルト1）
                null     // 最大ロット数
            );

            // Assert: 必要数量そのまま75個
            orderQuantity.Should().Be(75m);
        }
    }

    public class LeadTimeCalculation : MrpServiceTests
    {
        public LeadTimeCalculation(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public void 納期からリードタイムを逆算して着手日を計算する()
        {
            // Arrange
            var dueDate = new DateOnly(2025, 1, 20);
            int leadTime = 5;

            // Act
            var startDate = _mrpService.CalculateStartDate(dueDate, leadTime, 0);

            // Assert: 1/20 - 5日 = 1/15
            startDate.Should().Be(new DateOnly(2025, 1, 15));
        }

        [Fact]
        [Trait("Category", "Integration")]
        public void 安全リードタイムも考慮して着手日を計算する()
        {
            // Arrange
            var dueDate = new DateOnly(2025, 1, 20);
            int leadTime = 5;
            int safetyLeadTime = 2;

            // Act
            var startDate = _mrpService.CalculateStartDate(dueDate, leadTime, safetyLeadTime);

            // Assert: 1/20 - 5日 - 2日 = 1/13
            startDate.Should().Be(new DateOnly(2025, 1, 13));
        }
    }

    public class NewOrderGeneration : MrpServiceTests
    {
        public NewOrderGeneration(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 不足分に対して購買オーダを生成できる()
        {
            // Arrange
            var part = new Item
            {
                ItemCode = "PART-ORDER-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "部品F",
                ItemCategory = ItemCategory.Part,
                LeadTime = 3,
                MinLotSize = 100m
            };
            await _itemRepository.SaveAsync(part);

            var shortageQuantity = 75m;
            var dueDate = new DateOnly(2025, 1, 20);

            // Act
            var newOrder = await _mrpService.CreateShortageOrderAsync(
                "PART-ORDER-001", shortageQuantity, dueDate, "WH-001", OrderType.Purchase
            );

            // Assert
            newOrder.Should().NotBeNull();
            newOrder.ItemCode.Should().Be("PART-ORDER-001");
            newOrder.OrderType.Should().Be(OrderType.Purchase);
            newOrder.PlanQuantity.Should().Be(100m); // 最小ロット適用
            newOrder.StartDate.Should().Be(new DateOnly(2025, 1, 17)); // 3日前
            newOrder.DueDate.Should().Be(dueDate);
            newOrder.OrderNumber.Should().StartWith("PO-");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 不足分に対して製造オーダを生成できる()
        {
            // Arrange
            var product = new Item
            {
                ItemCode = "PROD-ORDER-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品G",
                ItemCategory = ItemCategory.Product,
                LeadTime = 5,
                MinLotSize = 50m,
                LotIncrement = 25m
            };
            await _itemRepository.SaveAsync(product);

            var shortageQuantity = 80m;
            var dueDate = new DateOnly(2025, 1, 25);

            // Act
            var newOrder = await _mrpService.CreateShortageOrderAsync(
                "PROD-ORDER-001", shortageQuantity, dueDate, "WH-001", OrderType.Manufacturing
            );

            // Assert
            newOrder.Should().NotBeNull();
            newOrder.ItemCode.Should().Be("PROD-ORDER-001");
            newOrder.OrderType.Should().Be(OrderType.Manufacturing);
            newOrder.PlanQuantity.Should().Be(100m); // 50 + 25*2 = 100
            newOrder.StartDate.Should().Be(new DateOnly(2025, 1, 20)); // 5日前
            newOrder.OrderNumber.Should().StartWith("MO-");
        }
    }

    public class RequiredQuantityCalculation : MrpServiceTests
    {
        public RequiredQuantityCalculation(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public void 不良率と歩留率を考慮した所要量を計算する()
        {
            // Arrange
            decimal parentQuantity = 100m;
            decimal baseQuantity = 1m;
            decimal requiredQuantity = 2m;
            decimal defectRate = 10m;   // 10%
            decimal yieldRate = 90m;    // 90%

            // Act
            var result = _mrpService.CalculateRequiredQuantity(
                parentQuantity, baseQuantity, requiredQuantity, defectRate, yieldRate);

            // 基本所要量: 100 × 2 / 1 = 200
            // 不良率考慮: 200 × 1.10 = 220
            // 歩留率考慮: 220 / 0.90 = 244.44... → 切り上げて 245
            result.Should().Be(245m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public void 歩留率100パーセントの場合は不良率のみ考慮()
        {
            // Arrange
            decimal parentQuantity = 100m;
            decimal baseQuantity = 1m;
            decimal requiredQuantity = 1m;
            decimal defectRate = 5m;    // 5%
            decimal yieldRate = 100m;   // 100%

            // Act
            var result = _mrpService.CalculateRequiredQuantity(
                parentQuantity, baseQuantity, requiredQuantity, defectRate, yieldRate);

            // 基本所要量: 100 × 1 / 1 = 100
            // 不良率考慮: 100 × 1.05 = 105
            // 歩留率考慮: 105 / 1.00 = 105
            result.Should().Be(105m);
        }
    }
}
