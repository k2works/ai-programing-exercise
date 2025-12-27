using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// オーダ情報リポジトリテスト
/// </summary>
[Collection("Database")]
public class OrderRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly IOrderRepository _orderRepository;
    private readonly IMpsRepository _mpsRepository;
    private readonly IItemRepository _itemRepository;
    private readonly IAllocationRepository _allocationRepository;
    private readonly IRequirementRepository _requirementRepository;
    private readonly IWorkOrderDetailRepository _workOrderDetailRepository;
    private readonly IWorkOrderRepository _workOrderRepository;

    public OrderRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _orderRepository = new OrderRepository(fixture.ConnectionString);
        _mpsRepository = new MpsRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _allocationRepository = new AllocationRepository(fixture.ConnectionString);
        _requirementRepository = new RequirementRepository(fixture.ConnectionString);
        _workOrderDetailRepository = new WorkOrderDetailRepository(fixture.ConnectionString);
        _workOrderRepository = new WorkOrderRepository(fixture.ConnectionString);

        // FK制約の順序に従って削除: 作業指示明細 → 作業指示 → 引当 → 所要 → オーダ → MPS → 品目
        _workOrderDetailRepository.DeleteAllAsync().Wait();
        _workOrderRepository.DeleteAllAsync().Wait();
        _allocationRepository.DeleteAllAsync().Wait();
        _requirementRepository.DeleteAllAsync().Wait();
        _orderRepository.DeleteAllAsync().Wait();
        _mpsRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    public class OrderRegistration : OrderRepositoryTests
    {
        public OrderRegistration(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 製造オーダを登録できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "PROD-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品A",
                ItemCategory = ItemCategory.Product
            };
            await _itemRepository.SaveAsync(item);

            // Act
            var order = new Order
            {
                OrderNumber = "MO-2025-001",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-001",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(order);

            // Assert
            order.Id.Should().NotBe(0);

            var result = await _orderRepository.FindByOrderNumberAsync("MO-2025-001");
            result.Should().NotBeNull();
            result!.OrderNumber.Should().Be("MO-2025-001");
            result.OrderType.Should().Be(OrderType.Manufacturing);
            result.PlanQuantity.Should().Be(100m);
            result.Status.Should().Be(PlanStatus.Draft);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 購買オーダを登録できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "PART-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "部品A",
                ItemCategory = ItemCategory.Part
            };
            await _itemRepository.SaveAsync(item);

            // Act
            var order = new Order
            {
                OrderNumber = "PO-2025-001",
                OrderType = OrderType.Purchase,
                ItemCode = "PART-001",
                StartDate = new DateOnly(2025, 1, 10),
                DueDate = new DateOnly(2025, 1, 15),
                PlanQuantity = 500m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(order);

            // Assert
            var result = await _orderRepository.FindByOrderNumberAsync("PO-2025-001");
            result.Should().NotBeNull();
            result!.OrderType.Should().Be(OrderType.Purchase);
        }
    }

    public class OrderHierarchy : OrderRepositoryTests
    {
        public OrderHierarchy(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task MPSに紐づくオーダを検索できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "PROD-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品B",
                ItemCategory = ItemCategory.Product
            };
            await _itemRepository.SaveAsync(item);

            var mps = new MasterProductionSchedule
            {
                MpsNumber = "MPS-2025-001",
                PlanDate = new DateOnly(2025, 1, 10),
                ItemCode = "PROD-002",
                PlanQuantity = 100m,
                DueDate = new DateOnly(2025, 1, 20),
                Status = PlanStatus.Confirmed
            };
            await _mpsRepository.SaveAsync(mps);

            var order = new Order
            {
                OrderNumber = "MO-2025-002",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-002",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                MpsId = mps.Id,
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(order);

            // Act
            var orders = await _orderRepository.FindByMpsIdAsync(mps.Id);

            // Assert
            orders.Should().HaveCount(1);
            orders[0].OrderNumber.Should().Be("MO-2025-002");
            orders[0].MpsId.Should().Be(mps.Id);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 親オーダに紐づく子オーダを検索できる()
        {
            // Arrange
            var product = new Item
            {
                ItemCode = "PROD-003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品C",
                ItemCategory = ItemCategory.Product
            };
            await _itemRepository.SaveAsync(product);

            var part = new Item
            {
                ItemCode = "PART-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "部品B",
                ItemCategory = ItemCategory.Part
            };
            await _itemRepository.SaveAsync(part);

            // 親オーダ（製品の製造オーダ）
            var parentOrder = new Order
            {
                OrderNumber = "MO-2025-003",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-003",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(parentOrder);

            // 子オーダ（部品の購買オーダ）
            var childOrder = new Order
            {
                OrderNumber = "PO-2025-002",
                OrderType = OrderType.Purchase,
                ItemCode = "PART-002",
                StartDate = new DateOnly(2025, 1, 10),
                DueDate = new DateOnly(2025, 1, 14),
                PlanQuantity = 200m,
                LocationCode = "WH-001",
                ParentOrderId = parentOrder.Id,
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(childOrder);

            // Act
            var childOrders = await _orderRepository.FindByParentOrderIdAsync(parentOrder.Id);

            // Assert
            childOrders.Should().HaveCount(1);
            childOrders[0].OrderNumber.Should().Be("PO-2025-002");
            childOrders[0].ParentOrderId.Should().Be(parentOrder.Id);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 親オーダIDを更新できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "PART-003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "部品C",
                ItemCategory = ItemCategory.Part
            };
            await _itemRepository.SaveAsync(item);

            var parentOrder = new Order
            {
                OrderNumber = "MO-2025-004",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PART-003",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(parentOrder);

            var childOrder = new Order
            {
                OrderNumber = "PO-2025-003",
                OrderType = OrderType.Purchase,
                ItemCode = "PART-003",
                StartDate = new DateOnly(2025, 1, 10),
                DueDate = new DateOnly(2025, 1, 14),
                PlanQuantity = 200m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(childOrder);

            // Act
            await _orderRepository.UpdateParentOrderIdAsync(childOrder.Id, parentOrder.Id);

            // Assert
            var updated = await _orderRepository.FindByIdAsync(childOrder.Id);
            updated.Should().NotBeNull();
            updated!.ParentOrderId.Should().Be(parentOrder.Id);
        }
    }

    public class StatusManagement : OrderRepositoryTests
    {
        public StatusManagement(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task オーダのステータスを変更できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "PROD-004",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品D",
                ItemCategory = ItemCategory.Product
            };
            await _itemRepository.SaveAsync(item);

            var order = new Order
            {
                OrderNumber = "MO-2025-005",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-004",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(order);

            // Act
            await _orderRepository.UpdateStatusAsync(order.Id, PlanStatus.Confirmed);

            // Assert
            var updated = await _orderRepository.FindByIdAsync(order.Id);
            updated.Should().NotBeNull();
            updated!.Status.Should().Be(PlanStatus.Confirmed);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 全てのオーダ種別を登録できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "ITEM-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "品目A",
                ItemCategory = ItemCategory.Part
            };
            await _itemRepository.SaveAsync(item);

            var orderTypes = Enum.GetValues<OrderType>();
            var index = 0;

            foreach (var orderType in orderTypes)
            {
                var order = new Order
                {
                    OrderNumber = $"ORD-TYPE-{index:D3}",
                    OrderType = orderType,
                    ItemCode = "ITEM-001",
                    StartDate = new DateOnly(2025, 1, 10),
                    DueDate = new DateOnly(2025, 1, 15),
                    PlanQuantity = 100m,
                    LocationCode = "WH-001",
                    Status = PlanStatus.Draft
                };

                await _orderRepository.SaveAsync(order);

                var result = await _orderRepository.FindByOrderNumberAsync(order.OrderNumber);
                result.Should().NotBeNull();
                result!.OrderType.Should().Be(orderType);

                index++;
            }
        }
    }
}
