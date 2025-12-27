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
/// 入荷・検収サービステスト
/// </summary>
[Collection("Database")]
public class ReceivingServiceTests
{
    private readonly PostgresFixture _fixture;
    private readonly ReceivingService _receivingService;
    private readonly PurchaseOrderService _purchaseOrderService;
    private readonly IReceivingRepository _receivingRepository;
    private readonly IInspectionRepository _inspectionRepository;
    private readonly IAcceptanceRepository _acceptanceRepository;
    private readonly IDefectRepository _defectRepository;
    private readonly IPurchaseOrderRepository _purchaseOrderRepository;
    private readonly IPurchaseOrderDetailRepository _purchaseOrderDetailRepository;
    private readonly IUnitPriceRepository _unitPriceRepository;
    private readonly IItemRepository _itemRepository;
    private readonly ISupplierRepository _supplierRepository;
    private readonly IConsumptionDetailRepository _consumptionDetailRepository;
    private readonly IConsumptionRepository _consumptionRepository;
    private readonly ISupplyDetailRepository _supplyDetailRepository;
    private readonly ISupplyRepository _supplyRepository;

    public ReceivingServiceTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _receivingRepository = new ReceivingRepository(fixture.ConnectionString);
        _inspectionRepository = new InspectionRepository(fixture.ConnectionString);
        _acceptanceRepository = new AcceptanceRepository(fixture.ConnectionString);
        _defectRepository = new DefectRepository(fixture.ConnectionString);
        _purchaseOrderRepository = new PurchaseOrderRepository(fixture.ConnectionString);
        _purchaseOrderDetailRepository = new PurchaseOrderDetailRepository(fixture.ConnectionString);
        _unitPriceRepository = new UnitPriceRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);
        _supplierRepository = new SupplierRepository(fixture.ConnectionString);
        _consumptionDetailRepository = new ConsumptionDetailRepository(fixture.ConnectionString);
        _consumptionRepository = new ConsumptionRepository(fixture.ConnectionString);
        _supplyDetailRepository = new SupplyDetailRepository(fixture.ConnectionString);
        _supplyRepository = new SupplyRepository(fixture.ConnectionString);

        _receivingService = new ReceivingService(
            _receivingRepository,
            _inspectionRepository,
            _acceptanceRepository,
            _purchaseOrderRepository,
            _purchaseOrderDetailRepository);

        _purchaseOrderService = new PurchaseOrderService(
            _purchaseOrderRepository,
            _purchaseOrderDetailRepository,
            _unitPriceRepository);

        // FK制約の順序に従って削除（消費・支給関連を先に削除）
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
        _defectRepository.DeleteAllAsync().Wait();
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
            SupplierCode = "SUP-RCV-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            SupplierName = "入荷テスト用仕入先",
            SupplierType = SupplierType.Vendor
        });

        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "MAT-RCV-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "入荷テスト用材料",
            ItemCategory = ItemCategory.Material
        });

        await _unitPriceRepository.SaveAsync(new UnitPrice
        {
            ItemCode = "MAT-RCV-001",
            SupplierCode = "SUP-RCV-001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            Price = 1000m
        });

        var createCommand = new PurchaseOrderCreateCommand
        {
            SupplierCode = "SUP-RCV-001",
            OrderDate = new DateOnly(2025, 1, 15),
            TaxRate = 10m,
            Details =
            [
                new PurchaseOrderDetailCommand
                {
                    ItemCode = "MAT-RCV-001",
                    OrderQuantity = 100m,
                    ExpectedReceivingDate = new DateOnly(2025, 1, 25)
                }
            ]
        };

        var purchaseOrder = await _purchaseOrderService.CreateOrderAsync(createCommand);
        await _purchaseOrderService.ConfirmOrderAsync(purchaseOrder.PurchaseOrderNumber);

        return purchaseOrder;
    }

    public class 入荷登録 : ReceivingServiceTests
    {
        public 入荷登録(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 発注済みの品目を入荷登録できる()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            var command = new ReceivingCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                ReceivingDate = new DateOnly(2025, 1, 20),
                ReceivingQuantity = 50m,
                ReceivingType = ReceivingType.Normal,
                DeliveryNoteNumber = "DN-001",
                LotNumber = "LOT-001",
                CreatedBy = "TestUser"
            };

            // Act
            var receiving = await _receivingService.RegisterReceivingAsync(command);

            // Assert
            receiving.Should().NotBeNull();
            receiving.ReceivingNumber.Should().StartWith("RCV-");
            receiving.ReceivingQuantity.Should().Be(50m);
            receiving.ItemCode.Should().Be("MAT-RCV-001");

            // 発注ステータスが「一部入荷」に更新されていることを確認
            var updatedOrder = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(
                purchaseOrder.PurchaseOrderNumber);
            updatedOrder!.Status.Should().Be(PurchaseOrderStatus.PartiallyReceived);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 全数入荷すると発注ステータスが入荷完了になる()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            var command = new ReceivingCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                ReceivingDate = new DateOnly(2025, 1, 20),
                ReceivingQuantity = 100m, // 全数
                ReceivingType = ReceivingType.Normal,
                CreatedBy = "TestUser"
            };

            // Act
            await _receivingService.RegisterReceivingAsync(command);

            // Assert
            var updatedOrder = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(
                purchaseOrder.PurchaseOrderNumber);
            updatedOrder!.Status.Should().Be(PurchaseOrderStatus.Received);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 残数量を超える入荷はエラー()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            var command = new ReceivingCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                ReceivingDate = new DateOnly(2025, 1, 20),
                ReceivingQuantity = 150m, // 発注数量(100)を超える
                ReceivingType = ReceivingType.Normal,
                CreatedBy = "TestUser"
            };

            // Act & Assert
            var act = async () => await _receivingService.RegisterReceivingAsync(command);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Receiving quantity*150*exceeds remaining quantity*100*");
        }
    }

    public class 受入検査 : ReceivingServiceTests
    {
        public 受入検査(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 入荷に対して検査を登録できる()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            var receivingCommand = new ReceivingCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                ReceivingDate = new DateOnly(2025, 1, 20),
                ReceivingQuantity = 100m,
                ReceivingType = ReceivingType.Normal
            };
            var receiving = await _receivingService.RegisterReceivingAsync(receivingCommand);

            var inspectionCommand = new InspectionCommand
            {
                ReceivingNumber = receiving.ReceivingNumber,
                InspectionDate = new DateOnly(2025, 1, 21),
                InspectionQuantity = 100m,
                PassedQuantity = 95m,
                FailedQuantity = 5m,
                InspectionResult = InspectionResult.Passed,
                InspectorCode = "INS-001",
                CreatedBy = "TestUser"
            };

            // Act
            var inspection = await _receivingService.RegisterInspectionAsync(inspectionCommand);

            // Assert
            inspection.Should().NotBeNull();
            inspection.InspectionNumber.Should().StartWith("INS-");
            inspection.PassedQuantity.Should().Be(95m);
            inspection.FailedQuantity.Should().Be(5m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 合格数と不合格数の合計が検査数と異なる場合はエラー()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            var receivingCommand = new ReceivingCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                ReceivingDate = new DateOnly(2025, 1, 20),
                ReceivingQuantity = 100m,
                ReceivingType = ReceivingType.Normal
            };
            var receiving = await _receivingService.RegisterReceivingAsync(receivingCommand);

            var inspectionCommand = new InspectionCommand
            {
                ReceivingNumber = receiving.ReceivingNumber,
                InspectionDate = new DateOnly(2025, 1, 21),
                InspectionQuantity = 100m,
                PassedQuantity = 90m,
                FailedQuantity = 5m, // 合計 95 != 100
                InspectionResult = InspectionResult.Passed
            };

            // Act & Assert
            var act = async () => await _receivingService.RegisterInspectionAsync(inspectionCommand);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Passed quantity (90) + Failed quantity (5) must equal Inspection quantity (100)");
        }
    }

    public class 検収処理 : ReceivingServiceTests
    {
        public 検収処理(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 検査合格品を検収できる()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            var receivingCommand = new ReceivingCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                ReceivingDate = new DateOnly(2025, 1, 20),
                ReceivingQuantity = 100m,
                ReceivingType = ReceivingType.Normal
            };
            var receiving = await _receivingService.RegisterReceivingAsync(receivingCommand);

            var inspectionCommand = new InspectionCommand
            {
                ReceivingNumber = receiving.ReceivingNumber,
                InspectionDate = new DateOnly(2025, 1, 21),
                InspectionQuantity = 100m,
                PassedQuantity = 100m,
                FailedQuantity = 0m,
                InspectionResult = InspectionResult.Passed
            };
            var inspection = await _receivingService.RegisterInspectionAsync(inspectionCommand);

            var acceptanceCommand = new AcceptanceCommand
            {
                InspectionNumber = inspection.InspectionNumber,
                AcceptanceDate = new DateOnly(2025, 1, 22),
                AcceptanceQuantity = 100m,
                AcceptanceAmount = 100000m, // 100 × 1000
                TaxAmount = 10000m,
                StorageLocationCode = "WH-001",
                CreatedBy = "TestUser"
            };

            // Act
            var acceptance = await _receivingService.ProcessAcceptanceAsync(acceptanceCommand);

            // Assert
            acceptance.Should().NotBeNull();
            acceptance.AcceptanceNumber.Should().StartWith("ACC-");
            acceptance.AcceptanceQuantity.Should().Be(100m);
            acceptance.AcceptanceAmount.Should().Be(100000m);

            // 発注ステータスが「検収完了」に更新されていることを確認
            var updatedOrder = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(
                purchaseOrder.PurchaseOrderNumber);
            updatedOrder!.Status.Should().Be(PurchaseOrderStatus.Accepted);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 検収数量が合格数量を超える場合はエラー()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            var receivingCommand = new ReceivingCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                ReceivingDate = new DateOnly(2025, 1, 20),
                ReceivingQuantity = 100m,
                ReceivingType = ReceivingType.Normal
            };
            var receiving = await _receivingService.RegisterReceivingAsync(receivingCommand);

            var inspectionCommand = new InspectionCommand
            {
                ReceivingNumber = receiving.ReceivingNumber,
                InspectionDate = new DateOnly(2025, 1, 21),
                InspectionQuantity = 100m,
                PassedQuantity = 95m,
                FailedQuantity = 5m,
                InspectionResult = InspectionResult.Passed
            };
            var inspection = await _receivingService.RegisterInspectionAsync(inspectionCommand);

            var acceptanceCommand = new AcceptanceCommand
            {
                InspectionNumber = inspection.InspectionNumber,
                AcceptanceDate = new DateOnly(2025, 1, 22),
                AcceptanceQuantity = 100m, // 合格数量(95)を超える
                AcceptanceAmount = 100000m
            };

            // Act & Assert
            var act = async () => await _receivingService.ProcessAcceptanceAsync(acceptanceCommand);
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Acceptance quantity*100*exceeds passed quantity*95*");
        }
    }

    public class 入荷から検収までの一連フロー : ReceivingServiceTests
    {
        public 入荷から検収までの一連フロー(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 分割入荷を含む一連の業務フローを実行できる()
        {
            // Arrange
            var purchaseOrder = await CreateTestPurchaseOrderAsync();

            // 1回目の入荷（分割）
            var firstReceiving = await _receivingService.RegisterReceivingAsync(new ReceivingCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                ReceivingDate = new DateOnly(2025, 1, 20),
                ReceivingQuantity = 60m,
                ReceivingType = ReceivingType.Split
            });

            // 1回目の検査
            var firstInspection = await _receivingService.RegisterInspectionAsync(new InspectionCommand
            {
                ReceivingNumber = firstReceiving.ReceivingNumber,
                InspectionDate = new DateOnly(2025, 1, 21),
                InspectionQuantity = 60m,
                PassedQuantity = 60m,
                FailedQuantity = 0m,
                InspectionResult = InspectionResult.Passed
            });

            // 1回目の検収
            var firstAcceptance = await _receivingService.ProcessAcceptanceAsync(new AcceptanceCommand
            {
                InspectionNumber = firstInspection.InspectionNumber,
                AcceptanceDate = new DateOnly(2025, 1, 22),
                AcceptanceQuantity = 60m,
                AcceptanceAmount = 60000m,
                TaxAmount = 6000m
            });

            // この時点ではまだ検収完了ではない
            var orderAfterFirst = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(
                purchaseOrder.PurchaseOrderNumber);
            orderAfterFirst!.Status.Should().NotBe(PurchaseOrderStatus.Accepted);

            // 2回目の入荷（残り）
            var secondReceiving = await _receivingService.RegisterReceivingAsync(new ReceivingCommand
            {
                PurchaseOrderNumber = purchaseOrder.PurchaseOrderNumber,
                LineNumber = 1,
                ReceivingDate = new DateOnly(2025, 1, 25),
                ReceivingQuantity = 40m,
                ReceivingType = ReceivingType.Normal
            });

            // 2回目の検査
            var secondInspection = await _receivingService.RegisterInspectionAsync(new InspectionCommand
            {
                ReceivingNumber = secondReceiving.ReceivingNumber,
                InspectionDate = new DateOnly(2025, 1, 26),
                InspectionQuantity = 40m,
                PassedQuantity = 40m,
                FailedQuantity = 0m,
                InspectionResult = InspectionResult.Passed
            });

            // 2回目の検収
            await _receivingService.ProcessAcceptanceAsync(new AcceptanceCommand
            {
                InspectionNumber = secondInspection.InspectionNumber,
                AcceptanceDate = new DateOnly(2025, 1, 27),
                AcceptanceQuantity = 40m,
                AcceptanceAmount = 40000m,
                TaxAmount = 4000m
            });

            // Assert: 全数検収完了
            var finalOrder = await _purchaseOrderRepository.FindByPurchaseOrderNumberAsync(
                purchaseOrder.PurchaseOrderNumber);
            finalOrder!.Status.Should().Be(PurchaseOrderStatus.Accepted);

            // 発注明細の数量確認
            var details = await _purchaseOrderDetailRepository.FindByPurchaseOrderNumberAsync(
                purchaseOrder.PurchaseOrderNumber);
            details[0].ReceivedQuantity.Should().Be(100m);
            details[0].InspectedQuantity.Should().Be(100m);
            details[0].AcceptedQuantity.Should().Be(100m);
        }
    }
}
