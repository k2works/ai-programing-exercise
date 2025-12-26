using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// 引当情報リポジトリテスト
/// </summary>
[Collection("Database")]
public class AllocationRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly IAllocationRepository _allocationRepository;
    private readonly IRequirementRepository _requirementRepository;
    private readonly IOrderRepository _orderRepository;
    private readonly IItemRepository _itemRepository;

    public AllocationRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _allocationRepository = new AllocationRepository(fixture.ConnectionString);
        _requirementRepository = new RequirementRepository(fixture.ConnectionString);
        _orderRepository = new OrderRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);

        _allocationRepository.DeleteAllAsync().Wait();
        _requirementRepository.DeleteAllAsync().Wait();
        _orderRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    public class AllocationRegistration : AllocationRepositoryTests
    {
        public AllocationRegistration(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 在庫引当を登録できる()
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

            var order = new Order
            {
                OrderNumber = "MO-2025-001",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PART-001",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(order);

            var requirement = new Requirement
            {
                RequirementNumber = "REQ-2025-001",
                OrderId = order.Id,
                ItemCode = "PART-001",
                DueDate = new DateOnly(2025, 1, 18),
                RequiredQuantity = 100m,
                AllocatedQuantity = 0m,
                ShortageQuantity = 100m,
                LocationCode = "WH-001"
            };
            await _requirementRepository.SaveAsync(requirement);

            // Act
            var allocation = new Allocation
            {
                RequirementId = requirement.Id,
                AllocationType = AllocationType.Inventory,
                AllocationDate = new DateOnly(2025, 1, 10),
                AllocatedQuantity = 50m,
                LocationCode = "WH-001"
            };
            await _allocationRepository.SaveAsync(allocation);

            // Assert
            allocation.Id.Should().NotBe(0);

            var allocations = await _allocationRepository.FindByRequirementIdAsync(requirement.Id);
            allocations.Should().HaveCount(1);
            allocations[0].AllocationType.Should().Be(AllocationType.Inventory);
            allocations[0].AllocatedQuantity.Should().Be(50m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 発注残引当を登録できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "PART-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "部品B",
                ItemCategory = ItemCategory.Part
            };
            await _itemRepository.SaveAsync(item);

            var order = new Order
            {
                OrderNumber = "MO-2025-002",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PART-002",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(order);

            var purchaseOrder = new Order
            {
                OrderNumber = "PO-2025-001",
                OrderType = OrderType.Purchase,
                ItemCode = "PART-002",
                StartDate = new DateOnly(2025, 1, 10),
                DueDate = new DateOnly(2025, 1, 14),
                PlanQuantity = 200m,
                LocationCode = "WH-001",
                Status = PlanStatus.Confirmed
            };
            await _orderRepository.SaveAsync(purchaseOrder);

            var requirement = new Requirement
            {
                RequirementNumber = "REQ-2025-002",
                OrderId = order.Id,
                ItemCode = "PART-002",
                DueDate = new DateOnly(2025, 1, 18),
                RequiredQuantity = 100m,
                AllocatedQuantity = 0m,
                ShortageQuantity = 100m,
                LocationCode = "WH-001"
            };
            await _requirementRepository.SaveAsync(requirement);

            // Act
            var allocation = new Allocation
            {
                RequirementId = requirement.Id,
                AllocationType = AllocationType.PurchaseOrder,
                OrderId = purchaseOrder.Id,
                AllocationDate = new DateOnly(2025, 1, 14),
                AllocatedQuantity = 100m,
                LocationCode = "WH-001"
            };
            await _allocationRepository.SaveAsync(allocation);

            // Assert
            var allocations = await _allocationRepository.FindByRequirementIdAsync(requirement.Id);
            allocations.Should().HaveCount(1);
            allocations[0].AllocationType.Should().Be(AllocationType.PurchaseOrder);
            allocations[0].OrderId.Should().Be(purchaseOrder.Id);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 複数の引当を登録できる()
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

            var order = new Order
            {
                OrderNumber = "MO-2025-003",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PART-003",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(order);

            var requirement = new Requirement
            {
                RequirementNumber = "REQ-2025-003",
                OrderId = order.Id,
                ItemCode = "PART-003",
                DueDate = new DateOnly(2025, 1, 18),
                RequiredQuantity = 100m,
                AllocatedQuantity = 0m,
                ShortageQuantity = 100m,
                LocationCode = "WH-001"
            };
            await _requirementRepository.SaveAsync(requirement);

            // Act: 在庫から50個、発注残から50個を引当
            var allocation1 = new Allocation
            {
                RequirementId = requirement.Id,
                AllocationType = AllocationType.Inventory,
                AllocationDate = new DateOnly(2025, 1, 10),
                AllocatedQuantity = 50m,
                LocationCode = "WH-001"
            };
            await _allocationRepository.SaveAsync(allocation1);

            var allocation2 = new Allocation
            {
                RequirementId = requirement.Id,
                AllocationType = AllocationType.PurchaseOrder,
                AllocationDate = new DateOnly(2025, 1, 14),
                AllocatedQuantity = 50m,
                LocationCode = "WH-001"
            };
            await _allocationRepository.SaveAsync(allocation2);

            // Assert
            var allocations = await _allocationRepository.FindByRequirementIdAsync(requirement.Id);
            allocations.Should().HaveCount(2);
            allocations.Sum(a => a.AllocatedQuantity).Should().Be(100m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 全ての引当区分を登録できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "PART-004",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "部品D",
                ItemCategory = ItemCategory.Part
            };
            await _itemRepository.SaveAsync(item);

            var order = new Order
            {
                OrderNumber = "MO-2025-004",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PART-004",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(order);

            var requirement = new Requirement
            {
                RequirementNumber = "REQ-2025-004",
                OrderId = order.Id,
                ItemCode = "PART-004",
                DueDate = new DateOnly(2025, 1, 18),
                RequiredQuantity = 300m,
                AllocatedQuantity = 0m,
                ShortageQuantity = 300m,
                LocationCode = "WH-001"
            };
            await _requirementRepository.SaveAsync(requirement);

            var allocationTypes = Enum.GetValues<AllocationType>();

            foreach (var allocationType in allocationTypes)
            {
                var allocation = new Allocation
                {
                    RequirementId = requirement.Id,
                    AllocationType = allocationType,
                    AllocationDate = new DateOnly(2025, 1, 10),
                    AllocatedQuantity = 100m,
                    LocationCode = "WH-001"
                };

                await _allocationRepository.SaveAsync(allocation);
            }

            // Assert
            var allocations = await _allocationRepository.FindByRequirementIdAsync(requirement.Id);
            allocations.Should().HaveCount(3);
            allocations.Select(a => a.AllocationType).Should().BeEquivalentTo(allocationTypes);
        }
    }
}
