using FluentAssertions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;
using Xunit;

namespace SalesManagement.Tests.IntegrationTests
{
    /// <summary>
    /// 顧客マスタのテスト
    /// </summary>
    public class CustomerTests : DatabaseTestBase
    {
        [Fact]
        public async Task 同一取引先に複数の顧客を枝番で登録できる()
        {
            // Arrange
            await SetupCompany("COMP001");
            var customerRepo = new CustomerRepository(ConnectionString);

            var customer1 = CreateTestCustomer("COMP001", 1, "本社営業部");
            var customer2 = CreateTestCustomer("COMP001", 2, "大阪支社");

            // Act
            await customerRepo.InsertAsync(customer1);
            await customerRepo.InsertAsync(customer2);

            // Assert
            var customers = (await customerRepo.FindByCustomerCodeAsync("COMP001")).ToList();
            customers.Should().HaveCount(2);
            customers[0].CustomerBranch.Should().Be(1);
            customers[1].CustomerBranch.Should().Be(2);
        }

        [Fact]
        public async Task 複合主キーで顧客を特定できる()
        {
            // Arrange
            await SetupCompany("COMP001");
            var customerRepo = new CustomerRepository(ConnectionString);

            var customer = CreateTestCustomer("COMP001", 1, "本社営業部");
            await customerRepo.InsertAsync(customer);

            // Act
            var found = await customerRepo.FindByIdAsync("COMP001", 1);

            // Assert
            found.Should().NotBeNull();
            found!.CustomerName.Should().Be("本社営業部");
        }

        [Fact]
        public async Task 顧客を更新できる()
        {
            // Arrange
            await SetupCompany("COMP001");
            var customerRepo = new CustomerRepository(ConnectionString);

            var customer = CreateTestCustomer("COMP001", 1, "本社営業部");
            await customerRepo.InsertAsync(customer);

            // Act
            customer.CustomerName = "本社営業部 改訂版";
            customer.CustomerTel = "03-1234-5678";
            customer.UpdatedAt = DateTime.Now;
            customer.UpdatedBy = "updater";
            await customerRepo.UpdateAsync(customer);

            // Assert
            var updated = await customerRepo.FindByIdAsync("COMP001", 1);
            updated.Should().NotBeNull();
            updated!.CustomerName.Should().Be("本社営業部 改訂版");
            updated.CustomerTel.Should().Be("03-1234-5678");
        }

        [Fact]
        public async Task 顧客を削除できる()
        {
            // Arrange
            await SetupCompany("COMP001");
            var customerRepo = new CustomerRepository(ConnectionString);

            var customer = CreateTestCustomer("COMP001", 1, "本社営業部");
            await customerRepo.InsertAsync(customer);

            // Act
            await customerRepo.DeleteAsync("COMP001", 1);

            // Assert
            var deleted = await customerRepo.FindByIdAsync("COMP001", 1);
            deleted.Should().BeNull();
        }

        // テストデータ作成ヘルパーメソッド
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
                CustomerNameKana = "ホンシャエイギョウブ",
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

        private async Task SetupCompany(string companyCode)
        {
            // 部門と社員を作成
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
