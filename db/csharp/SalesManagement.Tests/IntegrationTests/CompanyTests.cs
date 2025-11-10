using FluentAssertions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;
using Xunit;

namespace SalesManagement.Tests.IntegrationTests
{
    /// <summary>
    /// 取引先マスタのテスト
    /// </summary>
    public class CompanyTests : DatabaseTestBase
    {
        [Fact]
        public async Task 取引先を登録できる()
        {
            // Arrange
            var groupRepo = new CompanyGroupRepository(ConnectionString);
            var companyRepo = new CompanyRepository(ConnectionString);

            var group = CreateTestGroup("GRP1", "大手企業");
            await groupRepo.InsertAsync(group);

            var company = CreateTestCompany("COMP001", "株式会社テスト商事", "GRP1");

            // Act
            await companyRepo.InsertAsync(company);

            // Assert
            var found = await companyRepo.FindByIdAsync("COMP001");
            found.Should().NotBeNull();
            found!.CompanyName.Should().Be("株式会社テスト商事");
        }

        [Fact]
        public async Task 取引先を顧客と仕入先の両方の役割で登録できる()
        {
            // Arrange
            var groupRepo = new CompanyGroupRepository(ConnectionString);
            var companyRepo = new CompanyRepository(ConnectionString);
            var customerRepo = new CustomerRepository(ConnectionString);
            var supplierRepo = new SupplierRepository(ConnectionString);

            // 前提：部門と社員が存在
            await SetupDepartmentAndEmployee();

            var group = CreateTestGroup("GRP1", "大手企業");
            await groupRepo.InsertAsync(group);

            // 取引先マスタに1件登録
            var company = CreateTestCompany("COMP001", "株式会社テスト商事", "GRP1");
            await companyRepo.InsertAsync(company);

            // Act - 顧客としての役割を登録
            var customer = CreateTestCustomer("COMP001", 1, "テスト商事 本社");
            await customerRepo.InsertAsync(customer);

            // Act - 仕入先としての役割を登録
            var supplier = CreateTestSupplier("COMP001", 1, "テスト商事 仕入部");
            await supplierRepo.InsertAsync(supplier);

            // Assert - 両方が取得できることを確認
            var customers = await customerRepo.FindByCompanyCodeAsync("COMP001");
            var suppliers = await supplierRepo.FindByCompanyCodeAsync("COMP001");

            customers.Should().HaveCount(1);
            suppliers.Should().HaveCount(1);
        }

        [Fact]
        public async Task 取引先をグループで検索できる()
        {
            // Arrange
            var groupRepo = new CompanyGroupRepository(ConnectionString);
            var companyRepo = new CompanyRepository(ConnectionString);

            var group = CreateTestGroup("GRP1", "大手企業");
            await groupRepo.InsertAsync(group);

            await companyRepo.InsertAsync(CreateTestCompany("COMP001", "テスト商事A", "GRP1"));
            await companyRepo.InsertAsync(CreateTestCompany("COMP002", "テスト商事B", "GRP1"));

            // Act
            var companies = (await companyRepo.FindByGroupCodeAsync("GRP1")).ToList();

            // Assert
            companies.Should().HaveCount(2);
        }

        [Fact]
        public async Task 取引先を更新できる()
        {
            // Arrange
            var groupRepo = new CompanyGroupRepository(ConnectionString);
            var companyRepo = new CompanyRepository(ConnectionString);

            var group = CreateTestGroup("GRP1", "大手企業");
            await groupRepo.InsertAsync(group);

            var company = CreateTestCompany("COMP001", "株式会社テスト商事", "GRP1");
            await companyRepo.InsertAsync(company);

            // Act
            company.CompanyName = "株式会社テスト商事 改訂版";
            company.MaxCredit = 5000000;
            company.UpdatedAt = DateTime.Now;
            company.UpdatedBy = "updater";
            await companyRepo.UpdateAsync(company);

            // Assert
            var updated = await companyRepo.FindByIdAsync("COMP001");
            updated.Should().NotBeNull();
            updated!.CompanyName.Should().Be("株式会社テスト商事 改訂版");
            updated.MaxCredit.Should().Be(5000000);
        }

        [Fact]
        public async Task 取引先を削除できる()
        {
            // Arrange
            var groupRepo = new CompanyGroupRepository(ConnectionString);
            var companyRepo = new CompanyRepository(ConnectionString);

            var group = CreateTestGroup("GRP1", "大手企業");
            await groupRepo.InsertAsync(group);

            var company = CreateTestCompany("COMP001", "株式会社テスト商事", "GRP1");
            await companyRepo.InsertAsync(company);

            // Act
            await companyRepo.DeleteAsync("COMP001");

            // Assert
            var deleted = await companyRepo.FindByIdAsync("COMP001");
            deleted.Should().BeNull();
        }

        // テストデータ作成ヘルパーメソッド
        private static CompanyGroup CreateTestGroup(string code, string name)
        {
            return new CompanyGroup
            {
                CompanyGroupCode = code,
                CompanyGroupName = name,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private static Company CreateTestCompany(string code, string name, string groupCode)
        {
            return new Company
            {
                CompanyCode = code,
                CompanyName = name,
                CompanyNameKana = "テストショウジ",
                SupplierType = 0,
                ZipCode = "1000001",
                State = "東京都",
                Address1 = "千代田区千代田1-1",
                Address2 = "テストビル10F",
                NoSalesFlag = 0,
                WideUseType = 0,
                CompanyGroupCode = groupCode,
                MaxCredit = 1000000,
                TempCreditUp = 0,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private static Customer CreateTestCustomer(string companyCode, int branch, string name)
        {
            return new Customer
            {
                CustomerCode = companyCode,
                CustomerBranch = branch,
                CustomerType = 0,
                ArCode = companyCode,
                ArBranch = branch,
                PayerCode = companyCode,
                PayerBranch = branch,
                CustomerName = name,
                CustomerNameKana = "テストショウジホンシャ",
                EmployeeCode = "EMP001",
                CustomerCloseDate1 = 31,
                CustomerPayMonths1 = 1,
                CustomerPayMethod1 = 1,
                CustomerCloseDate2 = 0,
                CustomerPayMonths2 = 0,
                CustomerPayMethod2 = 1,
                CustomerArType = 0,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private static Supplier CreateTestSupplier(string companyCode, int branch, string name)
        {
            return new Supplier
            {
                SupplierCode = companyCode,
                SupplierBranch = branch,
                SupplierName = name,
                SupplierNameKana = "テストショウジシイレブ",
                SupplierCloseDate = 31,
                SupplierPayMonths = 2,
                SupplierPayMethod = 1,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private async Task SetupDepartmentAndEmployee()
        {
            var deptRepo = new DepartmentRepository(ConnectionString);
            var empRepo = new EmployeeRepository(ConnectionString);

            var dept = new Department
            {
                DepartmentCode = "DEPT01",
                DepartmentName = "営業部",
                OrganizationLevel = 1,
                DepartmentPath = "DEPT01",
                LowestLevelFlag = 1,
                SlipInputFlag = 1,
                StartDate = DateTime.Parse("2025-01-01"),
                EndDate = DateTime.Parse("9999-12-31"),
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
            await deptRepo.InsertAsync(dept);

            var emp = new Employee
            {
                EmployeeCode = "EMP001",
                EmployeeName = "山田太郎",
                EmployeeNameKana = "ヤマダタロウ",
                Gender = "M",
                BirthDate = DateTime.Parse("1990-01-01"),
                JoinDate = DateTime.Parse("2020-04-01"),
                DepartmentCode = "DEPT01",
                PositionCode = "POS01",
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
            await empRepo.InsertAsync(emp);
        }
    }
}
