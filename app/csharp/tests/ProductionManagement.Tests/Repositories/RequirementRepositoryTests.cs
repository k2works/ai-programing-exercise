using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// 所要情報リポジトリテスト
/// </summary>
[Collection("Database")]
public class RequirementRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly IRequirementRepository _requirementRepository;
    private readonly IOrderRepository _orderRepository;
    private readonly IItemRepository _itemRepository;
    private readonly IAllocationRepository _allocationRepository;

    public RequirementRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _requirementRepository = new RequirementRepository(fixture.ConnectionString);
        _orderRepository = new OrderRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _allocationRepository = new AllocationRepository(fixture.ConnectionString);

        // FK制約の順序に従って削除: 引当 → 所要 → オーダ → 品目
        _allocationRepository.DeleteAllAsync().Wait();
        _requirementRepository.DeleteAllAsync().Wait();
        _orderRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    public class RequirementRegistration : RequirementRepositoryTests
    {
        public RequirementRegistration(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 所要情報を登録できる()
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

            // Act
            var requirement = new Requirement
            {
                RequirementNumber = "REQ-2025-001",
                OrderId = order.Id,
                ItemCode = "PROD-001",
                DueDate = new DateOnly(2025, 1, 18),
                RequiredQuantity = 200m,
                AllocatedQuantity = 0m,
                ShortageQuantity = 200m,
                LocationCode = "WH-001"
            };
            await _requirementRepository.SaveAsync(requirement);

            // Assert
            requirement.Id.Should().NotBe(0);

            var result = await _requirementRepository.FindByIdAsync(requirement.Id);
            result.Should().NotBeNull();
            result!.RequirementNumber.Should().Be("REQ-2025-001");
            result.RequiredQuantity.Should().Be(200m);
            result.ShortageQuantity.Should().Be(200m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task オーダIDで所要情報を検索できる()
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

            var order = new Order
            {
                OrderNumber = "MO-2025-002",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-002",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(order);

            var req1 = new Requirement
            {
                RequirementNumber = "REQ-2025-002",
                OrderId = order.Id,
                ItemCode = "PART-001",
                DueDate = new DateOnly(2025, 1, 16),
                RequiredQuantity = 200m,
                AllocatedQuantity = 0m,
                ShortageQuantity = 200m,
                LocationCode = "WH-001"
            };
            await _requirementRepository.SaveAsync(req1);

            var req2 = new Requirement
            {
                RequirementNumber = "REQ-2025-003",
                OrderId = order.Id,
                ItemCode = "PART-002",
                DueDate = new DateOnly(2025, 1, 17),
                RequiredQuantity = 150m,
                AllocatedQuantity = 0m,
                ShortageQuantity = 150m,
                LocationCode = "WH-001"
            };
            await _requirementRepository.SaveAsync(req2);

            // Act
            var requirements = await _requirementRepository.FindByOrderIdAsync(order.Id);

            // Assert
            requirements.Should().HaveCount(2);
            requirements[0].DueDate.Should().BeBefore(requirements[1].DueDate);
        }
    }

    public class AllocationUpdate : RequirementRepositoryTests
    {
        public AllocationUpdate(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 引当済数量と不足数量を更新できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "PROD-003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品C",
                ItemCategory = ItemCategory.Product
            };
            await _itemRepository.SaveAsync(item);

            var order = new Order
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
            await _orderRepository.SaveAsync(order);

            var requirement = new Requirement
            {
                RequirementNumber = "REQ-2025-004",
                OrderId = order.Id,
                ItemCode = "PART-001",
                DueDate = new DateOnly(2025, 1, 18),
                RequiredQuantity = 100m,
                AllocatedQuantity = 0m,
                ShortageQuantity = 100m,
                LocationCode = "WH-001"
            };
            await _requirementRepository.SaveAsync(requirement);

            // Act: 50個を引当
            await _requirementRepository.UpdateAllocationAsync(requirement.Id, 50m, 50m);

            // Assert
            var updated = await _requirementRepository.FindByIdAsync(requirement.Id);
            updated.Should().NotBeNull();
            updated!.AllocatedQuantity.Should().Be(50m);
            updated.ShortageQuantity.Should().Be(50m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 全数引当で不足数量がゼロになる()
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
                OrderNumber = "MO-2025-004",
                OrderType = OrderType.Manufacturing,
                ItemCode = "PROD-004",
                StartDate = new DateOnly(2025, 1, 15),
                DueDate = new DateOnly(2025, 1, 20),
                PlanQuantity = 100m,
                LocationCode = "WH-001",
                Status = PlanStatus.Draft
            };
            await _orderRepository.SaveAsync(order);

            var requirement = new Requirement
            {
                RequirementNumber = "REQ-2025-005",
                OrderId = order.Id,
                ItemCode = "PART-001",
                DueDate = new DateOnly(2025, 1, 18),
                RequiredQuantity = 100m,
                AllocatedQuantity = 0m,
                ShortageQuantity = 100m,
                LocationCode = "WH-001"
            };
            await _requirementRepository.SaveAsync(requirement);

            // Act: 全数引当
            await _requirementRepository.UpdateAllocationAsync(requirement.Id, 100m, 0m);

            // Assert
            var updated = await _requirementRepository.FindByIdAsync(requirement.Id);
            updated.Should().NotBeNull();
            updated!.AllocatedQuantity.Should().Be(100m);
            updated.ShortageQuantity.Should().Be(0m);
        }
    }
}
