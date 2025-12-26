using FluentAssertions;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Services;

/// <summary>
/// 発注サービステスト
/// </summary>
[Collection("Database")]
public class PurchaseOrderServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly PurchaseOrderService _purchaseOrderService;
    private readonly IPurchaseOrderRepository _purchaseOrderRepository;
    private readonly IPurchaseOrderDetailRepository _purchaseOrderDetailRepository;
    private readonly IUnitPriceRepository _unitPriceRepository;
    private readonly IItemRepository _itemRepository;
    private readonly ISupplierRepository _supplierRepository;

    public PurchaseOrderServiceTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _purchaseOrderRepository = new PurchaseOrderRepository(fixture.ConnectionString);
        _purchaseOrderDetailRepository = new PurchaseOrderDetailRepository(fixture.ConnectionString);
        _unitPriceRepository = new UnitPriceRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _supplierRepository = new SupplierRepository(fixture.ConnectionString);

        _purchaseOrderService = new PurchaseOrderService(
            _purchaseOrderRepository,
            _purchaseOrderDetailRepository,
            _unitPriceRepository);

        // FK制約の順序に従って削除
        _purchaseOrderDetailRepository.DeleteAllAsync().Wait();
        _purchaseOrderRepository.DeleteAllAsync().Wait();
        _unitPriceRepository.DeleteAllAsync().Wait();
        _supplierRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    public class 発注データの作成 : PurchaseOrderServiceTests
    {
        public 発注データの作成(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 購買オーダから発注データを作成できる()
        {
            // Arrange: マスタデータを準備
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "テスト仕入先株式会社",
                SupplierType = SupplierType.Vendor
            });

            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料A",
                ItemCategory = ItemCategory.Material
            });

            await _unitPriceRepository.SaveAsync(new UnitPrice
            {
                ItemCode = "MAT-001",
                SupplierCode = "SUP-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                Price = 1000m
            });

            // Act: 発注を作成
            var input = new PurchaseOrderCreateCommand
            {
                SupplierCode = "SUP-001",
                OrderDate = new DateOnly(2025, 1, 15),
                TaxRate = 10m,
                Details =
                [
                    new PurchaseOrderDetailCommand
                    {
                        ItemCode = "MAT-001",
                        OrderQuantity = 100m,
                        ExpectedReceivingDate = new DateOnly(2025, 1, 25)
                    }
                ]
            };

            var purchaseOrder = await _purchaseOrderService.CreatePurchaseOrderAsync(input);

            // Assert
            purchaseOrder.Should().NotBeNull();
            purchaseOrder.PurchaseOrderNumber.Should().StartWith("PO-");
            purchaseOrder.Status.Should().Be(PurchaseOrderStatus.Creating);
            purchaseOrder.Details.Should().HaveCount(1);
            purchaseOrder.Details[0].OrderQuantity.Should().Be(100m);
            purchaseOrder.Details[0].OrderUnitPrice.Should().Be(1000m);
            purchaseOrder.Details[0].OrderAmount.Should().Be(100000m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 複数明細の発注を作成できる()
        {
            // Arrange
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "複数品目仕入先",
                SupplierType = SupplierType.Vendor
            });

            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料B",
                ItemCategory = ItemCategory.Material
            });

            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料C",
                ItemCategory = ItemCategory.Material
            });

            await _unitPriceRepository.SaveAsync(new UnitPrice
            {
                ItemCode = "MAT-002",
                SupplierCode = "SUP-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                Price = 500m
            });

            await _unitPriceRepository.SaveAsync(new UnitPrice
            {
                ItemCode = "MAT-003",
                SupplierCode = "SUP-002",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                Price = 800m
            });

            // Act
            var input = new PurchaseOrderCreateCommand
            {
                SupplierCode = "SUP-002",
                OrderDate = new DateOnly(2025, 1, 15),
                Details =
                [
                    new PurchaseOrderDetailCommand
                    {
                        ItemCode = "MAT-002",
                        OrderQuantity = 50m,
                        ExpectedReceivingDate = new DateOnly(2025, 1, 25)
                    },
                    new PurchaseOrderDetailCommand
                    {
                        ItemCode = "MAT-003",
                        OrderQuantity = 30m,
                        ExpectedReceivingDate = new DateOnly(2025, 1, 28)
                    }
                ]
            };

            var purchaseOrder = await _purchaseOrderService.CreatePurchaseOrderAsync(input);

            // Assert
            purchaseOrder.Details.Should().HaveCount(2);
            purchaseOrder.Details[0].LineNumber.Should().Be(1);
            purchaseOrder.Details[1].LineNumber.Should().Be(2);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 発注を発注済ステータスに変更できる()
        {
            // Arrange
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "ステータス確認用仕入先",
                SupplierType = SupplierType.Vendor
            });

            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-004",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料D",
                ItemCategory = ItemCategory.Material
            });

            await _unitPriceRepository.SaveAsync(new UnitPrice
            {
                ItemCode = "MAT-004",
                SupplierCode = "SUP-003",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                Price = 2000m
            });

            var input = new PurchaseOrderCreateCommand
            {
                SupplierCode = "SUP-003",
                OrderDate = new DateOnly(2025, 1, 15),
                Details =
                [
                    new PurchaseOrderDetailCommand
                    {
                        ItemCode = "MAT-004",
                        OrderQuantity = 10m,
                        ExpectedReceivingDate = new DateOnly(2025, 1, 25)
                    }
                ]
            };

            var purchaseOrder = await _purchaseOrderService.CreatePurchaseOrderAsync(input);

            // Act: 発注確定
            var confirmedOrder = await _purchaseOrderService.ConfirmPurchaseOrderAsync(
                purchaseOrder.PurchaseOrderNumber);

            // Assert
            confirmedOrder.Status.Should().Be(PurchaseOrderStatus.Ordered);
        }
    }

    public class 消費税計算 : PurchaseOrderServiceTests
    {
        public 消費税計算(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 消費税額が正しく計算される()
        {
            // Arrange
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-004",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "消費税確認用仕入先",
                SupplierType = SupplierType.Vendor
            });

            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-005",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料E",
                ItemCategory = ItemCategory.Material
            });

            await _unitPriceRepository.SaveAsync(new UnitPrice
            {
                ItemCode = "MAT-005",
                SupplierCode = "SUP-004",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                Price = 1000m
            });

            // Act
            var input = new PurchaseOrderCreateCommand
            {
                SupplierCode = "SUP-004",
                OrderDate = new DateOnly(2025, 1, 15),
                TaxRate = 10m,
                Details =
                [
                    new PurchaseOrderDetailCommand
                    {
                        ItemCode = "MAT-005",
                        OrderQuantity = 100m,
                        ExpectedReceivingDate = new DateOnly(2025, 1, 25)
                    }
                ]
            };

            var purchaseOrder = await _purchaseOrderService.CreatePurchaseOrderAsync(input);

            // Assert: 100 × 1000 = 100,000円、消費税 10,000円
            purchaseOrder.Details[0].OrderAmount.Should().Be(100000m);
            purchaseOrder.Details[0].TaxAmount.Should().Be(10000m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task デフォルト消費税率は10パーセント()
        {
            // Arrange
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-005",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "デフォルト税率確認用仕入先",
                SupplierType = SupplierType.Vendor
            });

            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-006",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料F",
                ItemCategory = ItemCategory.Material
            });

            await _unitPriceRepository.SaveAsync(new UnitPrice
            {
                ItemCode = "MAT-006",
                SupplierCode = "SUP-005",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                Price = 500m
            });

            // Act: TaxRate を指定しない
            var input = new PurchaseOrderCreateCommand
            {
                SupplierCode = "SUP-005",
                OrderDate = new DateOnly(2025, 1, 15),
                Details =
                [
                    new PurchaseOrderDetailCommand
                    {
                        ItemCode = "MAT-006",
                        OrderQuantity = 200m,
                        ExpectedReceivingDate = new DateOnly(2025, 1, 25)
                    }
                ]
            };

            var purchaseOrder = await _purchaseOrderService.CreatePurchaseOrderAsync(input);

            // Assert: 200 × 500 = 100,000円、消費税 10,000円（10%）
            purchaseOrder.Details[0].OrderAmount.Should().Be(100000m);
            purchaseOrder.Details[0].TaxAmount.Should().Be(10000m);
        }
    }

    public class 発注取消 : PurchaseOrderServiceTests
    {
        public 発注取消(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 未入荷の発注を取消できる()
        {
            // Arrange
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-006",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "取消確認用仕入先",
                SupplierType = SupplierType.Vendor
            });

            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-007",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料G",
                ItemCategory = ItemCategory.Material
            });

            await _unitPriceRepository.SaveAsync(new UnitPrice
            {
                ItemCode = "MAT-007",
                SupplierCode = "SUP-006",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                Price = 1500m
            });

            var input = new PurchaseOrderCreateCommand
            {
                SupplierCode = "SUP-006",
                OrderDate = new DateOnly(2025, 1, 15),
                Details =
                [
                    new PurchaseOrderDetailCommand
                    {
                        ItemCode = "MAT-007",
                        OrderQuantity = 50m,
                        ExpectedReceivingDate = new DateOnly(2025, 1, 25)
                    }
                ]
            };

            var purchaseOrder = await _purchaseOrderService.CreatePurchaseOrderAsync(input);

            // Act
            var cancelledOrder = await _purchaseOrderService.CancelPurchaseOrderAsync(
                purchaseOrder.PurchaseOrderNumber);

            // Assert
            cancelledOrder.Status.Should().Be(PurchaseOrderStatus.Cancelled);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 単価が登録されていない場合はエラー()
        {
            // Arrange
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "SUP-007",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "単価未登録確認用仕入先",
                SupplierType = SupplierType.Vendor
            });

            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-008",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料H",
                ItemCategory = ItemCategory.Material
            });
            // 単価は登録しない

            var input = new PurchaseOrderCreateCommand
            {
                SupplierCode = "SUP-007",
                OrderDate = new DateOnly(2025, 1, 15),
                Details =
                [
                    new PurchaseOrderDetailCommand
                    {
                        ItemCode = "MAT-008",
                        OrderQuantity = 50m,
                        ExpectedReceivingDate = new DateOnly(2025, 1, 25)
                    }
                ]
            };

            // Act & Assert
            var act = async () => await _purchaseOrderService.CreatePurchaseOrderAsync(input);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Unit price not found: MAT-008 / SUP-007");
        }
    }
}
