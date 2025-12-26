using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// 基準生産計画（MPS）リポジトリテスト
/// </summary>
[Collection("Database")]
public class MpsRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly IMpsRepository _mpsRepository;
    private readonly IItemRepository _itemRepository;

    public MpsRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _mpsRepository = new MpsRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);

        _mpsRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    public class MpsRegistration : MpsRepositoryTests
    {
        public MpsRegistration(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 基準生産計画を登録できる()
        {
            // Arrange: 品目を事前登録
            var item = new Item
            {
                ItemCode = "PROD-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品A",
                ItemCategory = ItemCategory.Product
            };
            await _itemRepository.SaveAsync(item);

            // Act: MPSを登録
            var mps = new MasterProductionSchedule
            {
                MpsNumber = "MPS-2025-001",
                PlanDate = new DateOnly(2025, 1, 10),
                ItemCode = "PROD-001",
                PlanQuantity = 100m,
                DueDate = new DateOnly(2025, 1, 20),
                Status = PlanStatus.Draft,
                LocationCode = "WH-001"
            };
            await _mpsRepository.SaveAsync(mps);

            // Assert
            mps.Id.Should().NotBe(0);

            var result = await _mpsRepository.FindByMpsNumberAsync("MPS-2025-001");
            result.Should().NotBeNull();
            result!.MpsNumber.Should().Be("MPS-2025-001");
            result.PlanQuantity.Should().Be(100m);
            result.Status.Should().Be(PlanStatus.Draft);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task MPSをIDで検索できる()
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
                MpsNumber = "MPS-2025-002",
                PlanDate = new DateOnly(2025, 1, 10),
                ItemCode = "PROD-002",
                PlanQuantity = 200m,
                DueDate = new DateOnly(2025, 1, 25),
                Status = PlanStatus.Draft
            };
            await _mpsRepository.SaveAsync(mps);

            // Act
            var result = await _mpsRepository.FindByIdAsync(mps.Id);

            // Assert
            result.Should().NotBeNull();
            result!.MpsNumber.Should().Be("MPS-2025-002");
            result.PlanQuantity.Should().Be(200m);
        }
    }

    public class StatusManagement : MpsRepositoryTests
    {
        public StatusManagement(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task MPSを確定ステータスに変更できる()
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

            var mps = new MasterProductionSchedule
            {
                MpsNumber = "MPS-2025-003",
                PlanDate = new DateOnly(2025, 1, 10),
                ItemCode = "PROD-003",
                PlanQuantity = 50m,
                DueDate = new DateOnly(2025, 1, 25),
                Status = PlanStatus.Draft
            };
            await _mpsRepository.SaveAsync(mps);

            // Act: ステータスを確定に変更
            await _mpsRepository.UpdateStatusAsync(mps.Id, PlanStatus.Confirmed);

            // Assert
            var confirmed = await _mpsRepository.FindByMpsNumberAsync("MPS-2025-003");
            confirmed.Should().NotBeNull();
            confirmed!.Status.Should().Be(PlanStatus.Confirmed);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task ステータスでMPSを検索できる()
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

            var mps1 = new MasterProductionSchedule
            {
                MpsNumber = "MPS-2025-004",
                PlanDate = new DateOnly(2025, 1, 10),
                ItemCode = "PROD-004",
                PlanQuantity = 100m,
                DueDate = new DateOnly(2025, 1, 20),
                Status = PlanStatus.Draft
            };
            await _mpsRepository.SaveAsync(mps1);

            var mps2 = new MasterProductionSchedule
            {
                MpsNumber = "MPS-2025-005",
                PlanDate = new DateOnly(2025, 1, 10),
                ItemCode = "PROD-004",
                PlanQuantity = 150m,
                DueDate = new DateOnly(2025, 1, 25),
                Status = PlanStatus.Confirmed
            };
            await _mpsRepository.SaveAsync(mps2);

            // Act
            var drafts = await _mpsRepository.FindByStatusAsync(PlanStatus.Draft);
            var confirmed = await _mpsRepository.FindByStatusAsync(PlanStatus.Confirmed);

            // Assert
            drafts.Should().HaveCount(1);
            drafts[0].MpsNumber.Should().Be("MPS-2025-004");

            confirmed.Should().HaveCount(1);
            confirmed[0].MpsNumber.Should().Be("MPS-2025-005");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 全ての計画ステータスを登録できる()
        {
            // Arrange
            var item = new Item
            {
                ItemCode = "PROD-005",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "製品E",
                ItemCategory = ItemCategory.Product
            };
            await _itemRepository.SaveAsync(item);

            var statuses = Enum.GetValues<PlanStatus>();
            var index = 0;

            foreach (var status in statuses)
            {
                var mps = new MasterProductionSchedule
                {
                    MpsNumber = $"MPS-STATUS-{index:D3}",
                    PlanDate = new DateOnly(2025, 1, 10),
                    ItemCode = "PROD-005",
                    PlanQuantity = 100m,
                    DueDate = new DateOnly(2025, 1, 20),
                    Status = status
                };

                await _mpsRepository.SaveAsync(mps);

                var result = await _mpsRepository.FindByMpsNumberAsync(mps.MpsNumber);
                result.Should().NotBeNull();
                result!.Status.Should().Be(status);

                index++;
            }
        }
    }
}
