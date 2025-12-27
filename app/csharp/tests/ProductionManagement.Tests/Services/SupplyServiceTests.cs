using FluentAssertions;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Domain.Models.Subcontract;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Services;

/// <summary>
/// 支給サービステスト
/// </summary>
[Collection("Database")]
public class SupplyServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly SupplyService _supplyService;
    private readonly PurchaseOrderService _purchaseOrderService;
    private readonly ISupplyRepository _supplyRepository;
    private readonly ISupplyDetailRepository _supplyDetailRepository;
    private readonly IConsumptionRepository _consumptionRepository;
    private readonly IConsumptionDetailRepository _consumptionDetailRepository;
    private readonly IPurchaseOrderRepository _purchaseOrderRepository;
    private readonly IPurchaseOrderDetailRepository _purchaseOrderDetailRepository;
    private readonly IUnitPriceRepository _unitPriceRepository;
    private readonly IItemRepository _itemRepository;
    private readonly ISupplierRepository _supplierRepository;
    private readonly IReceivingRepository _receivingRepository;
    private readonly IInspectionRepository _inspectionRepository;
    private readonly IAcceptanceRepository _acceptanceRepository;

    public SupplyServiceTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _supplyRepository = new SupplyRepository(fixture.ConnectionString);
        _supplyDetailRepository = new SupplyDetailRepository(fixture.ConnectionString);
        _consumptionRepository = new ConsumptionRepository(fixture.ConnectionString);
        _consumptionDetailRepository = new ConsumptionDetailRepository(fixture.ConnectionString);
        _purchaseOrderRepository = new PurchaseOrderRepository(fixture.ConnectionString);
        _purchaseOrderDetailRepository = new PurchaseOrderDetailRepository(fixture.ConnectionString);
        _unitPriceRepository = new UnitPriceRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _supplierRepository = new SupplierRepository(fixture.ConnectionString);
        _receivingRepository = new ReceivingRepository(fixture.ConnectionString);
        _inspectionRepository = new InspectionRepository(fixture.ConnectionString);
        _acceptanceRepository = new AcceptanceRepository(fixture.ConnectionString);

        _supplyService = new SupplyService(
            _supplyRepository,
            _supplyDetailRepository);

        _purchaseOrderService = new PurchaseOrderService(
            _purchaseOrderRepository,
            _purchaseOrderDetailRepository,
            _unitPriceRepository);

        // FK制約の順序に従って削除
        _consumptionDetailRepository.DeleteAllAsync().Wait();
        _consumptionRepository.DeleteAllAsync().Wait();
        _supplyDetailRepository.DeleteAllAsync().Wait();
        _supplyRepository.DeleteAllAsync().Wait();
        _acceptanceRepository.DeleteAllAsync().Wait();
        _inspectionRepository.DeleteAllAsync().Wait();
        _receivingRepository.DeleteAllAsync().Wait();
        _purchaseOrderDetailRepository.DeleteAllAsync().Wait();
        _purchaseOrderRepository.DeleteAllAsync().Wait();
        _unitPriceRepository.DeleteAllAsync().Wait();
        _supplierRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    /// <summary>
    /// テスト用の発注データを作成する
    /// </summary>
    private async Task<PurchaseOrder> CreateTestPurchaseOrderAsync()
    {
        await _supplierRepository.SaveAsync(new Supplier
        {
            SupplierCode = "SUB-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            SupplierName = "株式会社メッキ工業",
            SupplierType = SupplierType.Subcontractor
        });

        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "PLATED-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "メッキ加工品",
            ItemCategory = ItemCategory.SemiProduct
        });

        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "PRESS-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "プレス部品",
            ItemCategory = ItemCategory.Part
        });

        await _unitPriceRepository.SaveAsync(new UnitPrice
        {
            ItemCode = "PLATED-001",
            SupplierCode = "SUB-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            Price = 500m
        });

        var createCommand = new PurchaseOrderCreateCommand
        {
            SupplierCode = "SUB-001",
            OrderDate = new DateOnly(2025, 1, 15),
            TaxRate = 10m,
            Details =
            [
                new PurchaseOrderDetailCommand
                {
                    ItemCode = "PLATED-001",
                    OrderQuantity = 100m,
                    ExpectedReceivingDate = new DateOnly(2025, 1, 25)
                }
            ]
        };

        var purchaseOrder = await _purchaseOrderService.CreateOrderAsync(createCommand);
        await _purchaseOrderService.ConfirmOrderAsync(purchaseOrder.PurchaseOrderNumber);

        return purchaseOrder;
    }

    public class 支給データ作成 : SupplyServiceTests
    {
        public 支給データ作成(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 発注に紐づく支給データを作成できる()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            var command = new SupplyCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                SupplierCode = "SUB-001",
                SupplyDate = new DateOnly(2025, 1, 16),
                SupplierPersonCode = "EMP001",
                SupplyType = SupplyType.Free,
                Details =
                [
                    new SupplyDetailCommand
                    {
                        ItemCode = "PRESS-001",
                        Quantity = 100m,
                        UnitPrice = 200m
                    }
                ],
                CreatedBy = "TestUser"
            };

            // Act
            var supply = await _supplyService.CreateSupplyAsync(command);

            // Assert
            supply.Should().NotBeNull();
            supply.SupplyNumber.Should().StartWith("SUP-");
            supply.SupplierCode.Should().Be("SUB-001");
            supply.SupplyType.Should().Be(SupplyType.Free);
            supply.Details.Should().HaveCount(1);
            supply.Details[0].Quantity.Should().Be(100m);
            supply.Details[0].Amount.Should().Be(20000m); // 100 × 200
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 有償支給を作成できる()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            var command = new SupplyCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                SupplierCode = "SUB-001",
                SupplyDate = new DateOnly(2025, 1, 16),
                SupplierPersonCode = "EMP001",
                SupplyType = SupplyType.Paid,
                Details =
                [
                    new SupplyDetailCommand
                    {
                        ItemCode = "PRESS-001",
                        Quantity = 100m,
                        UnitPrice = 200m
                    }
                ]
            };

            // Act
            var supply = await _supplyService.CreateSupplyAsync(command);

            // Assert
            supply.SupplyType.Should().Be(SupplyType.Paid);
            supply.Details[0].Amount.Should().Be(20000m);
        }
    }

    public class 支給データ検索 : SupplyServiceTests
    {
        public 支給データ検索(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 支給番号で検索できる()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            var command = new SupplyCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                SupplierCode = "SUB-001",
                SupplyDate = new DateOnly(2025, 1, 16),
                SupplierPersonCode = "EMP001",
                Details =
                [
                    new SupplyDetailCommand
                    {
                        ItemCode = "PRESS-001",
                        Quantity = 50m,
                        UnitPrice = 200m
                    }
                ]
            };
            var createdSupply = await _supplyService.CreateSupplyAsync(command);

            // Act
            var supply = await _supplyService.FindBySupplyNumberAsync(createdSupply.SupplyNumber);

            // Assert
            supply.Should().NotBeNull();
            supply!.SupplyNumber.Should().Be(createdSupply.SupplyNumber);
            supply.Details.Should().HaveCount(1);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 発注明細で検索できる()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            var command1 = new SupplyCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                SupplierCode = "SUB-001",
                SupplyDate = new DateOnly(2025, 1, 16),
                SupplierPersonCode = "EMP001",
                Details =
                [
                    new SupplyDetailCommand
                    {
                        ItemCode = "PRESS-001",
                        Quantity = 50m,
                        UnitPrice = 200m
                    }
                ]
            };
            await _supplyService.CreateSupplyAsync(command1);

            var command2 = new SupplyCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                SupplierCode = "SUB-001",
                SupplyDate = new DateOnly(2025, 1, 17),
                SupplierPersonCode = "EMP001",
                Details =
                [
                    new SupplyDetailCommand
                    {
                        ItemCode = "PRESS-001",
                        Quantity = 50m,
                        UnitPrice = 200m
                    }
                ]
            };
            await _supplyService.CreateSupplyAsync(command2);

            // Act
            var supplies = await _supplyService.FindByPurchaseOrderDetailAsync(
                purchaseOrder.PurchaseOrderNumber, 1);

            // Assert
            supplies.Should().HaveCount(2);
        }
    }
}
