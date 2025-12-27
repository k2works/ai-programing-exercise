using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Quality;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// 品質管理リポジトリテスト
/// </summary>
[Collection("Database")]
public class QualityRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly IDefectMasterRepository _defectRepository;
    private readonly IShipmentInspectionRepository _inspectionRepository;
    private readonly ILotRepository _lotRepository;
    private readonly IItemRepository _itemRepository;

    public QualityRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _defectRepository = new DefectMasterRepository(fixture.ConnectionString);
        _inspectionRepository = new ShipmentInspectionRepository(fixture.ConnectionString);
        _lotRepository = new LotRepository(fixture.ConnectionString);
        _itemRepository = new ItemRepository(fixture.ConnectionString);

        // クリーンアップ
        _inspectionRepository.DeleteAllResultsAsync().Wait();
        _inspectionRepository.DeleteAllAsync().Wait();
        _defectRepository.DeleteAllAsync().Wait();
        _lotRepository.DeleteAllCompositionsAsync().Wait();
        _lotRepository.DeleteAllAsync().Wait();
        _itemRepository.DeleteAllAsync().Wait();
    }

    private async Task SetupItemDataAsync()
    {
        await _itemRepository.SaveAsync(new Item
        {
            ItemCode = "PROD001",
            EffectiveFrom = new DateOnly(2025, 1, 1),
            ItemName = "製品A",
            ItemCategory = ItemCategory.Product
        });
    }

    public class 欠点マスタ : QualityRepositoryTests
    {
        public 欠点マスタ(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 欠点マスタを登録して取得できる()
        {
            // Arrange
            var defect = new Defect
            {
                DefectCode = "DEF001",
                DefectName = "キズ",
                DefectCategory = "外観"
            };

            // Act
            await _defectRepository.SaveAsync(defect);
            var result = await _defectRepository.FindByDefectCodeAsync("DEF001");

            // Assert
            result.Should().NotBeNull();
            result!.DefectCode.Should().Be("DEF001");
            result.DefectName.Should().Be("キズ");
            result.DefectCategory.Should().Be("外観");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 全欠点を取得できる()
        {
            // Arrange
            await _defectRepository.SaveAsync(new Defect
            {
                DefectCode = "DEF001",
                DefectName = "キズ",
                DefectCategory = "外観"
            });
            await _defectRepository.SaveAsync(new Defect
            {
                DefectCode = "DEF002",
                DefectName = "変色",
                DefectCategory = "外観"
            });

            // Act
            var result = await _defectRepository.FindAllAsync();

            // Assert
            result.Should().HaveCount(2);
        }
    }

    public class 出荷検査 : QualityRepositoryTests
    {
        public 出荷検査(PostgresFixture fixture) : base(fixture) { }

        private async Task SetupDefectDataAsync()
        {
            await _defectRepository.SaveAsync(new Defect
            {
                DefectCode = "DEF001",
                DefectName = "キズ",
                DefectCategory = "外観"
            });
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 出荷検査を登録して取得できる()
        {
            // Arrange
            await SetupItemDataAsync();
            var inspection = new ShipmentInspection
            {
                InspectionNumber = "SI-202506-0001",
                ShipmentNumber = "SH-202506-0001",
                ItemCode = "PROD001",
                InspectionDate = new DateOnly(2025, 6, 15),
                InspectorCode = "EMP001",
                InspectionQuantity = 100m,
                PassedQuantity = 98m,
                FailedQuantity = 2m,
                Judgment = InspectionJudgment.Passed
            };

            // Act
            await _inspectionRepository.SaveAsync(inspection);
            var result = await _inspectionRepository.FindByInspectionNumberAsync("SI-202506-0001");

            // Assert
            result.Should().NotBeNull();
            result!.InspectionNumber.Should().Be("SI-202506-0001");
            result.ShipmentNumber.Should().Be("SH-202506-0001");
            result.ItemCode.Should().Be("PROD001");
            result.InspectionQuantity.Should().Be(100m);
            result.PassedQuantity.Should().Be(98m);
            result.FailedQuantity.Should().Be(2m);
            result.Judgment.Should().Be(InspectionJudgment.Passed);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 出荷番号で検査を検索できる()
        {
            // Arrange
            await SetupItemDataAsync();
            await _inspectionRepository.SaveAsync(new ShipmentInspection
            {
                InspectionNumber = "SI-202506-0001",
                ShipmentNumber = "SH-202506-0001",
                ItemCode = "PROD001",
                InspectionDate = new DateOnly(2025, 6, 15),
                InspectorCode = "EMP001",
                InspectionQuantity = 100m,
                PassedQuantity = 100m,
                FailedQuantity = 0m,
                Judgment = InspectionJudgment.Passed
            });

            // Act
            var result = await _inspectionRepository.FindByShipmentNumberAsync("SH-202506-0001");

            // Assert
            result.Should().HaveCount(1);
            result[0].InspectionNumber.Should().Be("SI-202506-0001");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 検査結果を登録して取得できる()
        {
            // Arrange
            await SetupItemDataAsync();
            await SetupDefectDataAsync();

            await _inspectionRepository.SaveAsync(new ShipmentInspection
            {
                InspectionNumber = "SI-202506-0001",
                ShipmentNumber = "SH-202506-0001",
                ItemCode = "PROD001",
                InspectionDate = new DateOnly(2025, 6, 15),
                InspectorCode = "EMP001",
                InspectionQuantity = 100m,
                PassedQuantity = 98m,
                FailedQuantity = 2m,
                Judgment = InspectionJudgment.Passed
            });

            var inspResult = new ShipmentInspectionResult
            {
                InspectionNumber = "SI-202506-0001",
                DefectCode = "DEF001",
                Quantity = 2m
            };

            // Act
            await _inspectionRepository.SaveResultAsync(inspResult);
            var results = await _inspectionRepository.FindResultsByInspectionNumberAsync("SI-202506-0001");

            // Assert
            results.Should().HaveCount(1);
            results[0].DefectCode.Should().Be("DEF001");
            results[0].Quantity.Should().Be(2m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 不合格判定で登録できる()
        {
            // Arrange
            await SetupItemDataAsync();
            var inspection = new ShipmentInspection
            {
                InspectionNumber = "SI-202506-0002",
                ShipmentNumber = "SH-202506-0002",
                ItemCode = "PROD001",
                InspectionDate = new DateOnly(2025, 6, 15),
                InspectorCode = "EMP001",
                InspectionQuantity = 100m,
                PassedQuantity = 50m,
                FailedQuantity = 50m,
                Judgment = InspectionJudgment.Failed
            };

            // Act
            await _inspectionRepository.SaveAsync(inspection);
            var result = await _inspectionRepository.FindByInspectionNumberAsync("SI-202506-0002");

            // Assert
            result.Should().NotBeNull();
            result!.Judgment.Should().Be(InspectionJudgment.Failed);
        }
    }

    public class ロット管理 : QualityRepositoryTests
    {
        public ロット管理(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task ロットを登録して取得できる()
        {
            // Arrange
            await SetupItemDataAsync();
            var lot = new LotMaster
            {
                LotNumber = "LOT-202506-0001",
                ItemCode = "PROD001",
                LotType = LotType.Manufactured,
                ManufactureDate = new DateOnly(2025, 6, 15),
                ExpirationDate = new DateOnly(2026, 6, 15),
                Quantity = 100m
            };

            // Act
            await _lotRepository.SaveAsync(lot);
            var result = await _lotRepository.FindByLotNumberAsync("LOT-202506-0001");

            // Assert
            result.Should().NotBeNull();
            result!.LotNumber.Should().Be("LOT-202506-0001");
            result.ItemCode.Should().Be("PROD001");
            result.LotType.Should().Be(LotType.Manufactured);
            result.ManufactureDate.Should().Be(new DateOnly(2025, 6, 15));
            result.ExpirationDate.Should().Be(new DateOnly(2026, 6, 15));
            result.Quantity.Should().Be(100m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 購入ロットを登録できる()
        {
            // Arrange
            await SetupItemDataAsync();
            var lot = new LotMaster
            {
                LotNumber = "LOT-P-202506-0001",
                ItemCode = "PROD001",
                LotType = LotType.Purchased,
                Quantity = 50m
            };

            // Act
            await _lotRepository.SaveAsync(lot);
            var result = await _lotRepository.FindByLotNumberAsync("LOT-P-202506-0001");

            // Assert
            result.Should().NotBeNull();
            result!.LotType.Should().Be(LotType.Purchased);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 品目コードでロットを検索できる()
        {
            // Arrange
            await SetupItemDataAsync();
            await _lotRepository.SaveAsync(new LotMaster
            {
                LotNumber = "LOT-202506-0001",
                ItemCode = "PROD001",
                LotType = LotType.Manufactured,
                Quantity = 100m
            });
            await _lotRepository.SaveAsync(new LotMaster
            {
                LotNumber = "LOT-202506-0002",
                ItemCode = "PROD001",
                LotType = LotType.Manufactured,
                Quantity = 50m
            });

            // Act
            var result = await _lotRepository.FindByItemCodeAsync("PROD001");

            // Assert
            result.Should().HaveCount(2);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task ロット構成を登録して取得できる()
        {
            // Arrange
            await SetupItemDataAsync();
            await _itemRepository.SaveAsync(new Item
            {
                ItemCode = "MAT001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                ItemName = "資材A",
                ItemCategory = ItemCategory.Material
            });

            // 親ロット（製品）
            await _lotRepository.SaveAsync(new LotMaster
            {
                LotNumber = "LOT-PROD-001",
                ItemCode = "PROD001",
                LotType = LotType.Manufactured,
                Quantity = 100m
            });

            // 子ロット（資材）
            await _lotRepository.SaveAsync(new LotMaster
            {
                LotNumber = "LOT-MAT-001",
                ItemCode = "MAT001",
                LotType = LotType.Purchased,
                Quantity = 200m
            });

            var composition = new LotComposition
            {
                ParentLotNumber = "LOT-PROD-001",
                ChildLotNumber = "LOT-MAT-001",
                UsedQuantity = 150m
            };

            // Act
            await _lotRepository.SaveCompositionAsync(composition);
            var childLots = await _lotRepository.FindChildLotsAsync("LOT-PROD-001");
            var parentLots = await _lotRepository.FindParentLotsAsync("LOT-MAT-001");

            // Assert
            childLots.Should().HaveCount(1);
            childLots[0].ChildLotNumber.Should().Be("LOT-MAT-001");
            childLots[0].UsedQuantity.Should().Be(150m);

            parentLots.Should().HaveCount(1);
            parentLots[0].ParentLotNumber.Should().Be("LOT-PROD-001");
        }
    }
}
