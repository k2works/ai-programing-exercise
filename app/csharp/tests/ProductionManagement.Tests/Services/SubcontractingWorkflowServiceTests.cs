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
/// 外注委託ワークフローサービステスト
/// </summary>
[Collection("Database")]
public class SubcontractingWorkflowServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly SubcontractingWorkflowService _workflowService;
    private readonly SupplyService _supplyService;
    private readonly ConsumptionService _consumptionService;
    private readonly ReceivingService _receivingService;
    private readonly PurchaseOrderService _purchaseOrderService;
    private readonly IPurchaseOrderRepository _purchaseOrderRepository;
    private readonly IPurchaseOrderDetailRepository _purchaseOrderDetailRepository;
    private readonly IUnitPriceRepository _unitPriceRepository;
    private readonly IItemRepository _itemRepository;
    private readonly ISupplierRepository _supplierRepository;
    private readonly ISupplyRepository _supplyRepository;
    private readonly ISupplyDetailRepository _supplyDetailRepository;
    private readonly IConsumptionRepository _consumptionRepository;
    private readonly IConsumptionDetailRepository _consumptionDetailRepository;
    private readonly IReceivingRepository _receivingRepository;
    private readonly IInspectionRepository _inspectionRepository;
    private readonly IAcceptanceRepository _acceptanceRepository;

    public SubcontractingWorkflowServiceTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _purchaseOrderRepository = new PurchaseOrderRepository(fixture.ConnectionString);
        _purchaseOrderDetailRepository = new PurchaseOrderDetailRepository(fixture.ConnectionString);
        _unitPriceRepository = new UnitPriceRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _supplierRepository = new SupplierRepository(fixture.ConnectionString);
        _supplyRepository = new SupplyRepository(fixture.ConnectionString);
        _supplyDetailRepository = new SupplyDetailRepository(fixture.ConnectionString);
        _consumptionRepository = new ConsumptionRepository(fixture.ConnectionString);
        _consumptionDetailRepository = new ConsumptionDetailRepository(fixture.ConnectionString);
        _receivingRepository = new ReceivingRepository(fixture.ConnectionString);
        _inspectionRepository = new InspectionRepository(fixture.ConnectionString);
        _acceptanceRepository = new AcceptanceRepository(fixture.ConnectionString);

        _workflowService = new SubcontractingWorkflowService(
            _purchaseOrderRepository,
            _purchaseOrderDetailRepository,
            _supplyRepository,
            _supplyDetailRepository,
            _consumptionDetailRepository,
            _unitPriceRepository);

        _purchaseOrderService = new PurchaseOrderService(
            _purchaseOrderRepository,
            _purchaseOrderDetailRepository,
            _unitPriceRepository);

        _supplyService = new SupplyService(
            _supplyRepository,
            _supplyDetailRepository);

        _receivingService = new ReceivingService(
            _receivingRepository,
            _inspectionRepository,
            _acceptanceRepository,
            _purchaseOrderRepository,
            _purchaseOrderDetailRepository);

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
    /// テスト用のマスタデータを作成する
    /// </summary>
    private async Task SetupMasterDataAsync()
    {
        await _supplierRepository.SaveAsync(new Supplier
        {
            SupplierCode = "SUB-WF-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            SupplierName = "株式会社メッキ工業",
            SupplierType = SupplierType.Subcontractor
        });

        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "PLATED-WF-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "メッキ加工品",
            ItemCategory = ItemCategory.SemiProduct
        });

        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "PRESS-WF-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "プレス部品（支給用）",
            ItemCategory = ItemCategory.Part
        });

        await _unitPriceRepository.SaveAsync(new UnitPrice
        {
            ItemCode = "PLATED-WF-001",
            SupplierCode = "SUB-WF-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            Price = 500m
        });
    }

    public class 外注発注作成 : SubcontractingWorkflowServiceTests
    {
        public 外注発注作成(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 外注発注を作成できる()
        {
            // Arrange
            await SetupMasterDataAsync();

            var command = new SubcontractOrderCommand
            {
                SupplierCode = "SUB-WF-001",
                ItemCode = "PLATED-WF-001",
                Quantity = 100m,
                DeliveryDate = new DateOnly(2025, 2, 15)
            };

            // Act
            var purchaseOrder = await _workflowService.CreateSubcontractOrderAsync(command);

            // Assert
            purchaseOrder.Should().NotBeNull();
            purchaseOrder.PurchaseOrderNumber.Should().StartWith("PO-");
            purchaseOrder.Status.Should().Be(PurchaseOrderStatus.Ordered);
            purchaseOrder.SupplierCode.Should().Be("SUB-WF-001");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 単価を指定して外注発注を作成できる()
        {
            // Arrange
            await SetupMasterDataAsync();

            var command = new SubcontractOrderCommand
            {
                SupplierCode = "SUB-WF-001",
                ItemCode = "PLATED-WF-001",
                Quantity = 100m,
                DeliveryDate = new DateOnly(2025, 2, 15),
                UnitPrice = 600m // カスタム単価
            };

            // Act
            var purchaseOrder = await _workflowService.CreateSubcontractOrderAsync(command);

            // Assert
            var details = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAsync(
                purchaseOrder.PurchaseOrderNumber);
            details.Should().HaveCount(1);
            details[0].OrderUnitPrice.Should().Be(600m);
            details[0].OrderAmount.Should().Be(60000m); // 100 × 600
        }
    }

    public class 外注委託状況取得 : SubcontractingWorkflowServiceTests
    {
        public 外注委託状況取得(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 発注直後の外注委託状況を取得できる()
        {
            // Arrange
            await SetupMasterDataAsync();

            var createCommand = new SubcontractOrderCommand
            {
                SupplierCode = "SUB-WF-001",
                ItemCode = "PLATED-WF-001",
                Quantity = 100m,
                DeliveryDate = new DateOnly(2025, 2, 15)
            };
            var purchaseOrder = await _workflowService.CreateSubcontractOrderAsync(createCommand);

            // Act
            var status = await _workflowService.GetSubcontractStatusAsync(purchaseOrder.PurchaseOrderNumber);

            // Assert
            status.Should().NotBeNull();
            status.PurchaseOrderNumber.Should().Be(purchaseOrder.PurchaseOrderNumber);
            status.Status.Should().Be(PurchaseOrderStatus.Ordered);
            status.SuppliedQuantity.Should().Be(0m);
            status.ConsumedQuantity.Should().Be(0m);
            status.AcceptedQuantity.Should().Be(0m);
            status.YieldRate.Should().Be(0m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 支給後の外注委託状況を取得できる()
        {
            // Arrange
            await SetupMasterDataAsync();

            // 外注発注を作成
            var createCommand = new SubcontractOrderCommand
            {
                SupplierCode = "SUB-WF-001",
                ItemCode = "PLATED-WF-001",
                Quantity = 100m,
                DeliveryDate = new DateOnly(2025, 2, 15)
            };
            var purchaseOrder = await _workflowService.CreateSubcontractOrderAsync(createCommand);

            // 支給を作成
            await _supplyService.CreateSupplyAsync(new SupplyCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                SupplierCode = "SUB-WF-001",
                SupplyDate = new DateOnly(2025, 1, 20),
                SupplierPersonCode = "EMP001",
                SupplyType = SupplyType.Free,
                Details =
                [
                    new SupplyDetailCommand
                    {
                        ItemCode = "PRESS-WF-001",
                        Quantity = 100m,
                        UnitPrice = 200m
                    }
                ]
            });

            // Act
            var status = await _workflowService.GetSubcontractStatusAsync(purchaseOrder.PurchaseOrderNumber);

            // Assert
            status.SuppliedQuantity.Should().Be(100m);
            status.ConsumedQuantity.Should().Be(0m);
            status.YieldRate.Should().Be(0m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 消費後の外注委託状況を取得できる()
        {
            // Arrange
            await SetupMasterDataAsync();

            // 外注発注を作成
            var createCommand = new SubcontractOrderCommand
            {
                SupplierCode = "SUB-WF-001",
                ItemCode = "PLATED-WF-001",
                Quantity = 100m,
                DeliveryDate = new DateOnly(2025, 2, 15)
            };
            var purchaseOrder = await _workflowService.CreateSubcontractOrderAsync(createCommand);

            // 支給を作成
            await _supplyService.CreateSupplyAsync(new SupplyCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                SupplierCode = "SUB-WF-001",
                SupplyDate = new DateOnly(2025, 1, 20),
                SupplierPersonCode = "EMP001",
                SupplyType = SupplyType.Free,
                Details =
                [
                    new SupplyDetailCommand
                    {
                        ItemCode = "PRESS-WF-001",
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
                ReceivingDate = new DateOnly(2025, 2, 10),
                ReceivingQuantity = 100m,
                ReceivingType = ReceivingType.Normal
            });

            // 消費を登録
            await _consumptionService.CreateConsumptionAsync(new ConsumptionCommand
            {
                ReceivingNumber = receiving.ReceivingNumber,
                ConsumptionDate = new DateOnly(2025, 2, 11),
                SupplierCode = "SUB-WF-001",
                Details =
                [
                    new ConsumptionDetailCommand
                    {
                        ItemCode = "PRESS-WF-001",
                        Quantity = 95m // 95%の歩留り
                    }
                ]
            });

            // Act
            var status = await _workflowService.GetSubcontractStatusAsync(purchaseOrder.PurchaseOrderNumber);

            // Assert
            status.SuppliedQuantity.Should().Be(100m);
            status.ConsumedQuantity.Should().Be(95m);
            status.YieldRate.Should().Be(0.95m); // 95/100
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 存在しない発注番号でエラー()
        {
            // Arrange
            await SetupMasterDataAsync();

            // Act & Assert
            var act = async () => await _workflowService.GetSubcontractStatusAsync("PO-NOTEXIST-0001");
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Purchase order not found*");
        }
    }
}
