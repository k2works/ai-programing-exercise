using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// 取引先リポジトリテスト
/// </summary>
[Collection("Database")]
public class SupplierRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly ISupplierRepository _supplierRepository;

    public SupplierRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _supplierRepository = new SupplierRepository(fixture.ConnectionString);
        _supplierRepository.DeleteAllAsync().Wait();
    }

    public class Registration : SupplierRepositoryTests
    {
        public Registration(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 取引先を登録できる()
        {
            // Arrange
            var supplier = new Supplier
            {
                SupplierCode = "SUP001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "株式会社テスト",
                SupplierNameKana = "カブシキガイシャテスト",
                SupplierType = SupplierType.Vendor,
                PostalCode = "100-0001",
                Address = "東京都千代田区1-1-1",
                PhoneNumber = "03-1234-5678",
                FaxNumber = "03-1234-5679",
                ContactPerson = "山田太郎"
            };

            // Act
            await _supplierRepository.SaveAsync(supplier);

            // Assert
            var result = await _supplierRepository.FindByCodeAsync("SUP001");
            result.Should().NotBeNull();
            result!.SupplierName.Should().Be("株式会社テスト");
            result.SupplierType.Should().Be(SupplierType.Vendor);
            result.PostalCode.Should().Be("100-0001");
            result.ContactPerson.Should().Be("山田太郎");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 同じ取引先コードでも適用開始日が異なれば登録できる()
        {
            // Arrange
            var supplier1 = new Supplier
            {
                SupplierCode = "SUP001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "株式会社テスト（旧）",
                SupplierType = SupplierType.Vendor
            };
            await _supplierRepository.SaveAsync(supplier1);

            var supplier2 = new Supplier
            {
                SupplierCode = "SUP001",
                EffectiveFrom = new DateOnly(2025, 4, 1),
                SupplierName = "株式会社テスト（新）",
                SupplierType = SupplierType.Vendor
            };

            // Act
            await _supplierRepository.SaveAsync(supplier2);

            // Assert: 最新の適用開始日のレコードが取得される
            var result = await _supplierRepository.FindByCodeAsync("SUP001");
            result.Should().NotBeNull();
            result!.SupplierName.Should().Be("株式会社テスト（新）");
            result.EffectiveFrom.Should().Be(new DateOnly(2025, 4, 1));
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 全ての取引先区分を登録できる()
        {
            var supplierTypes = Enum.GetValues<SupplierType>();
            var index = 0;

            foreach (var supplierType in supplierTypes)
            {
                var supplier = new Supplier
                {
                    SupplierCode = $"SUP-{index:D3}",
                    EffectiveFrom = new DateOnly(2025, 1, 1),
                    SupplierName = $"取引先{supplierType.GetDisplayName()}",
                    SupplierType = supplierType
                };

                await _supplierRepository.SaveAsync(supplier);

                var result = await _supplierRepository.FindByCodeAsync(supplier.SupplierCode);
                result.Should().NotBeNull();
                result!.SupplierType.Should().Be(supplierType);

                index++;
            }
        }
    }

    public class EffectiveDateQuery : SupplierRepositoryTests
    {
        public EffectiveDateQuery(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 基準日で有効な取引先を検索できる()
        {
            // Arrange: 世代管理された取引先を登録
            var supplier1 = new Supplier
            {
                SupplierCode = "SUP001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                EffectiveTo = new DateOnly(2025, 4, 1),
                SupplierName = "株式会社テスト（旧）",
                SupplierType = SupplierType.Vendor
            };
            await _supplierRepository.SaveAsync(supplier1);

            var supplier2 = new Supplier
            {
                SupplierCode = "SUP001",
                EffectiveFrom = new DateOnly(2025, 4, 1),
                SupplierName = "株式会社テスト（新）",
                SupplierType = SupplierType.Vendor
            };
            await _supplierRepository.SaveAsync(supplier2);

            // Act & Assert: 2月時点では旧情報
            var resultFeb = await _supplierRepository.FindByCodeAndDateAsync("SUP001", new DateOnly(2025, 2, 15));
            resultFeb.Should().NotBeNull();
            resultFeb!.SupplierName.Should().Be("株式会社テスト（旧）");

            // Act & Assert: 5月時点では新情報
            var resultMay = await _supplierRepository.FindByCodeAndDateAsync("SUP001", new DateOnly(2025, 5, 15));
            resultMay.Should().NotBeNull();
            resultMay!.SupplierName.Should().Be("株式会社テスト（新）");
        }
    }

    public class TypeQuery : SupplierRepositoryTests
    {
        public TypeQuery(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 取引先区分で取引先を検索できる()
        {
            // Arrange
            var suppliers = new[]
            {
                new Supplier { SupplierCode = "V001", EffectiveFrom = new DateOnly(2025, 1, 1), SupplierName = "仕入先1", SupplierType = SupplierType.Vendor },
                new Supplier { SupplierCode = "V002", EffectiveFrom = new DateOnly(2025, 1, 1), SupplierName = "仕入先2", SupplierType = SupplierType.Vendor },
                new Supplier { SupplierCode = "S001", EffectiveFrom = new DateOnly(2025, 1, 1), SupplierName = "外注先1", SupplierType = SupplierType.Subcontractor },
                new Supplier { SupplierCode = "C001", EffectiveFrom = new DateOnly(2025, 1, 1), SupplierName = "得意先1", SupplierType = SupplierType.Customer }
            };

            foreach (var supplier in suppliers)
            {
                await _supplierRepository.SaveAsync(supplier);
            }

            // Act
            var vendors = await _supplierRepository.FindByTypeAsync(SupplierType.Vendor);

            // Assert
            vendors.Should().HaveCount(2);
            vendors.Should().OnlyContain(s => s.SupplierType == SupplierType.Vendor);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 仕入先兼外注先を検索できる()
        {
            // Arrange
            var supplier = new Supplier
            {
                SupplierCode = "VS001",
                EffectiveFrom = new DateOnly(2025, 1, 1),
                SupplierName = "仕入先兼外注先1",
                SupplierType = SupplierType.VendorAndSubcontractor
            };
            await _supplierRepository.SaveAsync(supplier);

            // Act
            var result = await _supplierRepository.FindByTypeAsync(SupplierType.VendorAndSubcontractor);

            // Assert
            result.Should().HaveCount(1);
            result[0].SupplierType.Should().Be(SupplierType.VendorAndSubcontractor);
        }
    }
}
