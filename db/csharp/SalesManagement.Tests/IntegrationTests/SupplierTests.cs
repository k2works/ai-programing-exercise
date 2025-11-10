using FluentAssertions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;
using Xunit;

namespace SalesManagement.Tests.IntegrationTests
{
    /// <summary>
    /// 仕入先マスタのテスト
    /// </summary>
    public class SupplierTests : DatabaseTestBase
    {
        [Fact]
        public async Task 同一取引先に複数の仕入先を枝番で登録できる()
        {
            // Arrange
            await SetupCompany("COMP001");
            var supplierRepo = new SupplierRepository(ConnectionString);

            var supplier1 = CreateTestSupplier("COMP001", 1, "本社仕入部");
            var supplier2 = CreateTestSupplier("COMP001", 2, "工場仕入部");

            // Act
            await supplierRepo.InsertAsync(supplier1);
            await supplierRepo.InsertAsync(supplier2);

            // Assert
            var suppliers = (await supplierRepo.FindBySupplierCodeAsync("COMP001")).ToList();
            suppliers.Should().HaveCount(2);
            suppliers[0].SupplierBranch.Should().Be(1);
            suppliers[1].SupplierBranch.Should().Be(2);
        }

        [Fact]
        public async Task 複合主キーで仕入先を特定できる()
        {
            // Arrange
            await SetupCompany("COMP001");
            var supplierRepo = new SupplierRepository(ConnectionString);

            var supplier = CreateTestSupplier("COMP001", 1, "本社仕入部");
            await supplierRepo.InsertAsync(supplier);

            // Act
            var found = await supplierRepo.FindByIdAsync("COMP001", 1);

            // Assert
            found.Should().NotBeNull();
            found!.SupplierName.Should().Be("本社仕入部");
        }

        [Fact]
        public async Task 仕入先を更新できる()
        {
            // Arrange
            await SetupCompany("COMP001");
            var supplierRepo = new SupplierRepository(ConnectionString);

            var supplier = CreateTestSupplier("COMP001", 1, "本社仕入部");
            await supplierRepo.InsertAsync(supplier);

            // Act
            supplier.SupplierName = "本社仕入部 改訂版";
            supplier.SupplierTel = "03-1234-5678";
            supplier.UpdatedAt = DateTime.Now;
            supplier.UpdatedBy = "updater";
            await supplierRepo.UpdateAsync(supplier);

            // Assert
            var updated = await supplierRepo.FindByIdAsync("COMP001", 1);
            updated.Should().NotBeNull();
            updated!.SupplierName.Should().Be("本社仕入部 改訂版");
            updated.SupplierTel.Should().Be("03-1234-5678");
        }

        [Fact]
        public async Task 仕入先を削除できる()
        {
            // Arrange
            await SetupCompany("COMP001");
            var supplierRepo = new SupplierRepository(ConnectionString);

            var supplier = CreateTestSupplier("COMP001", 1, "本社仕入部");
            await supplierRepo.InsertAsync(supplier);

            // Act
            await supplierRepo.DeleteAsync("COMP001", 1);

            // Assert
            var deleted = await supplierRepo.FindByIdAsync("COMP001", 1);
            deleted.Should().BeNull();
        }

        // テストデータ作成ヘルパーメソッド
        private static Supplier CreateTestSupplier(string companyCode, int branch, string name)
        {
            return new Supplier
            {
                SupplierCode = companyCode,
                SupplierBranch = branch,
                SupplierName = name,
                SupplierNameKana = "ホンシャシイレブ",
                SupplierCloseDate = 31,
                SupplierPayMonths = 2,
                SupplierPayMethod = 1,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private async Task SetupCompany(string companyCode)
        {
            // グループと取引先を作成
            var groupRepo = new CompanyGroupRepository(ConnectionString);
            var companyRepo = new CompanyRepository(ConnectionString);

            var group = new CompanyGroup
            {
                CompanyGroupCode = "GRP1",
                CompanyGroupName = "大手企業",
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
            await groupRepo.InsertAsync(group);

            var company = new Company
            {
                CompanyCode = companyCode,
                CompanyName = "株式会社テスト商事",
                CompanyNameKana = "テストショウジ",
                SupplierType = 0,
                NoSalesFlag = 0,
                WideUseType = 0,
                CompanyGroupCode = "GRP1",
                MaxCredit = 1000000,
                TempCreditUp = 0,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
            await companyRepo.InsertAsync(company);
        }
    }
}
