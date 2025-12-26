using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// 発注リポジトリテスト
/// </summary>
[Collection("Database")]
public class PurchaseOrderRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly IPurchaseOrderRepository _purchaseOrderRepository;
    private readonly IPurchaseOrderDetailRepository _purchaseOrderDetailRepository;
    private readonly IUnitPriceRepository _unitPriceRepository;
    private readonly IItemRepository _itemRepository;
    private readonly ISupplierRepository _supplierRepository;

    public PurchaseOrderRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _purchaseOrderRepository = new PurchaseOrderRepository(fixture.ConnectionString);
        _purchaseOrderDetailRepository = new PurchaseOrderDetailRepository(fixture.ConnectionString);
        _unitPriceRepository = new UnitPriceRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _supplierRepository = new SupplierRepository(fixture.ConnectionString);

        // FK制約の順序に従って削除
        _purchaseOrderDetailRepository.DeleteAllAsync().Wait();
        _purchaseOrderRepository.DeleteAllAsync().Wait();
        _unitPriceRepository.DeleteAllAsync().Wait();
        _supplierRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    public class Registration : PurchaseOrderRepositoryTests
    {
        public Registration(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 発注を登録できる()
        {
            // Arrange
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "テスト仕入先",
                SupplierType = SupplierType.Vendor
            });

            // Act
            var purchaseOrder = new PurchaseOrder
            {
                PurchaseOrderNumber = "PO-202501-0001",
                OrderDate = new DateOnly(2025, 1, 15),
                SupplierCode = "SUP-001",
                Status = PurchaseOrderStatus.Creating
            };
            await _purchaseOrderRepository.SaveAsync(purchaseOrder);

            // Assert
            purchaseOrder.Id.Should().NotBe(0);

            var result = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync("PO-202501-0001");
            result.Should().NotBeNull();
            result!.PurchaseOrderNumber.Should().Be("PO-202501-0001");
            result.Status.Should().Be(PurchaseOrderStatus.Creating);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 発注明細を登録できる()
        {
            // Arrange
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料A",
                ItemCategory = ItemCategory.Material
            });

            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "テスト仕入先2",
                SupplierType = SupplierType.Vendor
            });

            var purchaseOrder = new PurchaseOrder
            {
                PurchaseOrderNumber = "PO-202501-0002",
                OrderDate = new DateOnly(2025, 1, 15),
                SupplierCode = "SUP-002",
                Status = PurchaseOrderStatus.Creating
            };
            await _purchaseOrderRepository.SaveAsync(purchaseOrder);

            // Act
            var detail = new PurchaseOrderDetail
            {
                PurchaseOrderNumber = "PO-202501-0002",
                LineNumber = 1,
                ItemCode = "MAT-001",
                ExpectedReceivingDate = new DateOnly(2025, 1, 25),
                OrderUnitPrice = 1000m,
                OrderQuantity = 100m,
                OrderAmount = 100000m,
                TaxAmount = 10000m
            };
            await _purchaseOrderDetailRepository.SaveAsync(detail);

            // Assert
            detail.Id.Should().NotBe(0);

            var details = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAsync("PO-202501-0002");
            details.Should().HaveCount(1);
            details[0].ItemCode.Should().Be("MAT-001");
            details[0].OrderQuantity.Should().Be(100m);
            details[0].OrderAmount.Should().Be(100000m);
        }
    }

    public class StatusUpdate : PurchaseOrderRepositoryTests
    {
        public StatusUpdate(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 発注ステータスを更新できる()
        {
            // Arrange
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "テスト仕入先3",
                SupplierType = SupplierType.Vendor
            });

            var purchaseOrder = new PurchaseOrder
            {
                PurchaseOrderNumber = "PO-202501-0003",
                OrderDate = new DateOnly(2025, 1, 15),
                SupplierCode = "SUP-003",
                Status = PurchaseOrderStatus.Creating
            };
            await _purchaseOrderRepository.SaveAsync(purchaseOrder);

            // Act
            await _purchaseOrderRepository.UpdateStatusAsync("PO-202501-0003", PurchaseOrderStatus.Ordered);

            // Assert
            var result = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync("PO-202501-0003");
            result.Should().NotBeNull();
            result!.Status.Should().Be(PurchaseOrderStatus.Ordered);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 全ての発注ステータスを設定できる()
        {
            // Arrange
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-004",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "テスト仕入先4",
                SupplierType = SupplierType.Vendor
            });

            var statuses = Enum.GetValues<PurchaseOrderStatus>();
            var index = 0;

            foreach (var status in statuses)
            {
                var purchaseOrder = new PurchaseOrder
                {
                    PurchaseOrderNumber = $"PO-STATUS-{index:D4}",
                    OrderDate = new DateOnly(2025, 1, 15),
                    SupplierCode = "SUP-004",
                    Status = status
                };
                await _purchaseOrderRepository.SaveAsync(purchaseOrder);

                var result = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(purchaseOrder.PurchaseOrderNumber);
                result.Should().NotBeNull();
                result!.Status.Should().Be(status);

                index++;
            }
        }
    }

    public class LatestNumberQuery : PurchaseOrderRepositoryTests
    {
        public LatestNumberQuery(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 最新の発注番号を取得できる()
        {
            // Arrange
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-005",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "テスト仕入先5",
                SupplierType = SupplierType.Vendor
            });

            await _purchaseOrderRepository.SaveAsync(new PurchaseOrder
            {
                PurchaseOrderNumber = "PO-202501-0001",
                OrderDate = new DateOnly(2025, 1, 15),
                SupplierCode = "SUP-005",
                Status = PurchaseOrderStatus.Creating
            });

            await _purchaseOrderRepository.SaveAsync(new PurchaseOrder
            {
                PurchaseOrderNumber = "PO-202501-0005",
                OrderDate = new DateOnly(2025, 1, 16),
                SupplierCode = "SUP-005",
                Status = PurchaseOrderStatus.Creating
            });

            await _purchaseOrderRepository.SaveAsync(new PurchaseOrder
            {
                PurchaseOrderNumber = "PO-202501-0003",
                OrderDate = new DateOnly(2025, 1, 17),
                SupplierCode = "SUP-005",
                Status = PurchaseOrderStatus.Creating
            });

            // Act
            var latestNumber = await _purchaseOrderRepository.FindLatestPurchaseOrderNumberAsync("PO-202501-%");

            // Assert
            latestNumber.Should().Be("PO-202501-0005");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 該当する発注番号がない場合はNullを返す()
        {
            // Act
            var latestNumber = await _purchaseOrderRepository.FindLatestPurchaseOrderNumberAsync("PO-202599-%");

            // Assert
            latestNumber.Should().BeNull();
        }
    }
}
