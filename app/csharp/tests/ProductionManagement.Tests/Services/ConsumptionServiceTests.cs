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
/// 消費サービステスト
/// </summary>
[Collection("Database")]
public class ConsumptionServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly ConsumptionService _consumptionService;
    private readonly SupplyService _supplyService;
    private readonly ReceivingService _receivingService;
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

    public ConsumptionServiceTests(PostgresFixture fixture)
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

        _purchaseOrderService = new PurchaseOrderService(
            _purchaseOrderRepository,
            _purchaseOrderDetailRepository,
            _unitPriceRepository);

        _receivingService = new ReceivingService(
            _receivingRepository,
            _inspectionRepository,
            _acceptanceRepository,
            _purchaseOrderRepository,
            _purchaseOrderDetailRepository);

        _supplyService = new SupplyService(
            _supplyRepository,
            _supplyDetailRepository);

        _consumptionService = new ConsumptionService(
            _consumptionRepository,
            _consumptionDetailRepository,
            _receivingRepository,
            _supplyRepository,
            _supplyDetailRepository);

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
    /// テスト用の発注・支給・入荷データを作成する
    /// </summary>
    private async Task<(PurchaseOrder order, Supply supply, Receiving receiving)> CreateTestDataAsync()
    {
        await _supplierRepository.SaveAsync(new Supplier
        {
            SupplierCode = "SUB-CON-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            SupplierName = "株式会社メッキ工業",
            SupplierType = SupplierType.Subcontractor
        });

        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "PLATED-CON-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "メッキ加工品",
            ItemCategory = ItemCategory.SemiProduct
        });

        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "PRESS-CON-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "プレス部品（支給用）",
            ItemCategory = ItemCategory.Part
        });

        await _unitPriceRepository.SaveAsync(new UnitPrice
        {
            ItemCode = "PLATED-CON-001",
            SupplierCode = "SUB-CON-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            Price = 500m
        });

        // 発注を作成
        var purchaseOrder = await _purchaseOrderService.CreatePurchaseOrderAsync(new PurchaseOrderCreateCommand
        {
            SupplierCode = "SUB-CON-001",
            OrderDate = new DateOnly(2025, 1, 15),
            TaxRate = 10m,
            Details =
            [
                new PurchaseOrderDetailCommand
                {
                    ItemCode = "PLATED-CON-001",
                    OrderQuantity = 100m,
                    ExpectedReceivingDate = new DateOnly(2025, 1, 25)
                }
            ]
        });
        await _purchaseOrderService.ConfirmPurchaseOrderAsync(purchaseOrder.PurchaseOrderNumber);

        // 支給を作成
        var supply = await _supplyService.CreateSupplyAsync(new SupplyCommand
        {
            PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
            LineNumber = 1,
            SupplierCode = "SUB-CON-001",
            SupplyDate = new DateOnly(2025, 1, 16),
            SupplierPersonCode = "EMP001",
            SupplyType = SupplyType.Free,
            Details =
            [
                new SupplyDetailCommand
                {
                    ItemCode = "PRESS-CON-001",
                    Quantity = 100m,
                    UnitPrice = 200m
                }
            ]
        });

        // 入荷を登録
        var receiving = await _receivingService.RegisterReceivingAsync(new ReceivingCommand
        {
            PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
            LineNumber = 1,
            ReceivingDate = new DateOnly(2025, 1, 25),
            ReceivingQuantity = 100m,
            ReceivingType = ReceivingType.Normal
        });

        return (purchaseOrder, supply, receiving);
    }

    public class 消費データ作成 : ConsumptionServiceTests
    {
        public 消費データ作成(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 入荷時に支給品の消費を記録できる()
        {
            // Arrange
            var (_, _, receiving) = await CreateTestDataAsync();

            var command = new ConsumptionCommand
            {
                ReceivingNumber = receiving.ReceivingNumber,
                ConsumptionDate = new DateOnly(2025, 1, 26),
                SupplierCode = "SUB-CON-001",
                Details =
                [
                    new ConsumptionDetailCommand
                    {
                        ItemCode = "PRESS-CON-001",
                        Quantity = 95m
                    }
                ],
                CreatedBy = "TestUser"
            };

            // Act
            var consumption = await _consumptionService.CreateConsumptionAsync(command);

            // Assert
            consumption.Should().NotBeNull();
            consumption.ConsumptionNumber.Should().StartWith("CON-");
            consumption.Details.Should().HaveCount(1);
            consumption.Details[0].Quantity.Should().Be(95m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 消費数量は支給数量を超えてはならない()
        {
            // Arrange
            var (_, _, receiving) = await CreateTestDataAsync();

            var command = new ConsumptionCommand
            {
                ReceivingNumber = receiving.ReceivingNumber,
                ConsumptionDate = new DateOnly(2025, 1, 26),
                SupplierCode = "SUB-CON-001",
                Details =
                [
                    new ConsumptionDetailCommand
                    {
                        ItemCode = "PRESS-CON-001",
                        Quantity = 150m // 支給数量(100)を超える
                    }
                ]
            };

            // Act & Assert
            var act = async () => await _consumptionService.CreateConsumptionAsync(command);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("*exceeds supply quantity*");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 支給がない入荷への消費登録はエラー()
        {
            // Arrange: 支給なしの発注・入荷を作成
            await _supplierRepository.SaveAsync(new Supplier
            {
                SupplierCode = "VEN-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "通常仕入先",
                SupplierType = SupplierType.Vendor
            });

            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "材料",
                ItemCategory = ItemCategory.Material
            });

            await _unitPriceRepository.SaveAsync(new UnitPrice
            {
                ItemCode = "MAT-001",
                SupplierCode = "VEN-001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                Price = 100m
            });

            var purchaseOrder = await _purchaseOrderService.CreatePurchaseOrderAsync(new PurchaseOrderCreateCommand
            {
                SupplierCode = "VEN-001",
                OrderDate = new DateOnly(2025, 1, 15),
                Details =
                [
                    new PurchaseOrderDetailCommand
                    {
                        ItemCode = "MAT-001",
                        OrderQuantity = 100m,
                        ExpectedReceivingDate = new DateOnly(2025, 1, 25)
                    }
                ]
            });
            await _purchaseOrderService.ConfirmPurchaseOrderAsync(purchaseOrder.PurchaseOrderNumber);

            var receiving = await _receivingService.RegisterReceivingAsync(new ReceivingCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                ReceivingDate = new DateOnly(2025, 1, 25),
                ReceivingQuantity = 100m,
                ReceivingType = ReceivingType.Normal
            });

            var command = new ConsumptionCommand
            {
                ReceivingNumber = receiving.ReceivingNumber,
                ConsumptionDate = new DateOnly(2025, 1, 26),
                SupplierCode = "VEN-001",
                Details =
                [
                    new ConsumptionDetailCommand
                    {
                        ItemCode = "MAT-001",
                        Quantity = 100m
                    }
                ]
            };

            // Act & Assert
            var act = async () => await _consumptionService.CreateConsumptionAsync(command);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Supply not found for receiving*");
        }
    }

    public class 消費率の計算 : ConsumptionServiceTests
    {
        public 消費率の計算(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 支給に対する消費率を計算できる()
        {
            // Arrange
            var (_, supply, receiving) = await CreateTestDataAsync();

            await _consumptionService.CreateConsumptionAsync(new ConsumptionCommand
            {
                ReceivingNumber = receiving.ReceivingNumber,
                ConsumptionDate = new DateOnly(2025, 1, 26),
                SupplierCode = "SUB-CON-001",
                Details =
                [
                    new ConsumptionDetailCommand
                    {
                        ItemCode = "PRESS-CON-001",
                        Quantity = 95m
                    }
                ]
            });

            // Act
            var rate = await _consumptionService.CalculateConsumptionRateAsync(
                supply.SupplyNumber, "PRESS-CON-001");

            // Assert: 95/100 = 0.95
            rate.Should().Be(0.95m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 消費がない場合は消費率0を返す()
        {
            // Arrange
            var (_, supply, _) = await CreateTestDataAsync();

            // Act: 消費を登録せずに消費率を計算
            var rate = await _consumptionService.CalculateConsumptionRateAsync(
                supply.SupplyNumber, "PRESS-CON-001");

            // Assert
            rate.Should().Be(0m);
        }
    }
}
