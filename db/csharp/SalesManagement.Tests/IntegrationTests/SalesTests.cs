using FluentAssertions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;
using Xunit;

namespace SalesManagement.Tests.IntegrationTests
{
    /// <summary>
    /// 売上データのテスト
    /// </summary>
    public class SalesTests : DatabaseTestBase
    {
        [Fact]
        public async Task 受注から売上を計上できる()
        {
            // Arrange
            await SetupOrderWithDetails("ORD001", "COMP001", 1, "PROD001");

            var salesRepo = new SalesRepository(ConnectionString);
            var sales = CreateTestSales("SAL001", "ORD001", "COMP001");

            // Act
            await salesRepo.InsertAsync(sales);

            // Assert
            var found = await salesRepo.FindByIdAsync("SAL001");
            found.Should().NotBeNull();
            found!.OrderNo.Should().Be("ORD001");
        }

        [Fact]
        public async Task 受注番号で売上を検索できる()
        {
            // Arrange
            await SetupOrderWithDetails("ORD001", "COMP001", 1, "PROD001");

            var salesRepo = new SalesRepository(ConnectionString);
            await salesRepo.InsertAsync(CreateTestSales("SAL001", "ORD001", "COMP001"));
            await salesRepo.InsertAsync(CreateTestSales("SAL002", "ORD001", "COMP001"));

            // Act
            var salesList = (await salesRepo.FindByOrderNoAsync("ORD001")).ToList();

            // Assert
            salesList.Should().HaveCount(2);
            salesList.All(s => s.OrderNo == "ORD001").Should().BeTrue();
        }

        [Fact]
        public async Task 売上と明細を一緒に登録できる()
        {
            // Arrange
            await SetupOrderWithDetails("ORD001", "COMP001", 1, "PROD001");

            var salesRepo = new SalesRepository(ConnectionString);
            var detailRepo = new SalesDetailRepository(ConnectionString);

            // ヘッダ登録
            var sales = CreateTestSales("SAL001", "ORD001", "COMP001");
            await salesRepo.InsertAsync(sales);

            // 明細登録
            var detail1 = CreateTestSalesDetail("SAL001", 1, "PROD001", "商品A", 10000, 5);
            var detail2 = CreateTestSalesDetail("SAL001", 2, "PROD001", "商品A", 10000, 3);

            // Act
            await detailRepo.InsertAsync(detail1);
            await detailRepo.InsertAsync(detail2);

            // Assert
            var details = (await detailRepo.FindBySalesNoAsync("SAL001")).ToList();
            details.Should().HaveCount(2);
            details.Sum(d => d.Quantity).Should().Be(8);
        }

        [Fact]
        public async Task 売上を削除すると明細も削除される()
        {
            // Arrange
            await SetupSalesWithDetails("SAL001", "ORD001", "COMP001", 1, "PROD001");

            var salesRepo = new SalesRepository(ConnectionString);
            var detailRepo = new SalesDetailRepository(ConnectionString);

            // Act
            await salesRepo.DeleteAsync("SAL001");

            // Assert
            var sales = await salesRepo.FindByIdAsync("SAL001");
            var details = await detailRepo.FindBySalesNoAsync("SAL001");

            sales.Should().BeNull();
            details.Should().BeEmpty(); // CASCADE DELETE
        }

        [Fact]
        public async Task 取引先別に売上を検索できる()
        {
            // Arrange
            await SetupOrderWithDetails("ORD001", "COMP001", 1, "PROD001");
            await SetupOrderWithDetails("ORD002", "COMP002", 1, "PROD001");

            var salesRepo = new SalesRepository(ConnectionString);

            await salesRepo.InsertAsync(CreateTestSales("SAL001", "ORD001", "COMP001"));
            await salesRepo.InsertAsync(CreateTestSales("SAL002", "ORD001", "COMP001"));
            await salesRepo.InsertAsync(CreateTestSales("SAL003", "ORD002", "COMP002"));

            // Act
            var salesList = (await salesRepo.FindByCompanyCodeAsync("COMP001")).ToList();

            // Assert
            salesList.Should().HaveCount(2);
            salesList.All(s => s.CompanyCode == "COMP001").Should().BeTrue();
        }

        // テストデータ作成ヘルパーメソッド
        private static Sales CreateTestSales(string salesNo, string orderNo, string companyCode)
        {
            return new Sales
            {
                SalesNo = salesNo,
                SalesDate = DateTime.Now,
                OrderNo = orderNo,
                DepartmentCode = "DEPT001",
                StartDate = DateTime.Parse("2025-01-01"),
                CompanyCode = companyCode,
                EmployeeCode = "EMP001",
                WarehouseCode = "WH1",
                SalesAmount = 100000,
                ConsumptionTax = 10000,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private static SalesDetail CreateTestSalesDetail(string salesNo, int salesRowNo, string productCode, string productName, int unitPrice, int quantity)
        {
            return new SalesDetail
            {
                SalesNo = salesNo,
                SalesRowNo = salesRowNo,
                ProductCode = productCode,
                ProductName = productName,
                UnitPrice = unitPrice,
                Quantity = quantity,
                ConsumptionTaxRate = 10,
                Discount = 0,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private async Task SetupOrderWithDetails(string orderNo, string customerCode, int customerBranch, string productCode)
        {
            await SetupCustomer(customerCode, customerBranch);
            await SetupProduct(productCode, "CAT001");

            var orderRepo = new OrderRepository(ConnectionString);
            var detailRepo = new OrderDetailRepository(ConnectionString);

            var order = CreateTestOrder(orderNo, customerCode, customerBranch);
            await orderRepo.InsertAsync(order);

            var detail = CreateTestOrderDetail(orderNo, 1, productCode, "テスト商品", 10000, 5);
            await detailRepo.InsertAsync(detail);
        }

        private async Task SetupSalesWithDetails(string salesNo, string orderNo, string customerCode, int customerBranch, string productCode)
        {
            await SetupOrderWithDetails(orderNo, customerCode, customerBranch, productCode);

            var salesRepo = new SalesRepository(ConnectionString);
            var detailRepo = new SalesDetailRepository(ConnectionString);

            var sales = CreateTestSales(salesNo, orderNo, customerCode);
            await salesRepo.InsertAsync(sales);

            var detail = CreateTestSalesDetail(salesNo, 1, productCode, "テスト商品", 10000, 5);
            await detailRepo.InsertAsync(detail);
        }

        private static Order CreateTestOrder(string orderNo, string customerCode, int customerBranch)
        {
            return new Order
            {
                OrderNo = orderNo,
                OrderDate = DateTime.Now,
                DepartmentCode = "DEPT001",
                StartDate = DateTime.Parse("2025-01-01"),
                CustomerCode = customerCode,
                CustomerBranch = customerBranch,
                EmployeeCode = "EMP001",
                WarehouseCode = "WH1",
                OrderAmount = 100000,
                ConsumptionTax = 10000,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private static OrderDetail CreateTestOrderDetail(string orderNo, int orderRowNo, string productCode, string productName, int unitPrice, int quantity)
        {
            return new OrderDetail
            {
                OrderNo = orderNo,
                OrderRowNo = orderRowNo,
                ProductCode = productCode,
                ProductName = productName,
                UnitPrice = unitPrice,
                Quantity = quantity,
                ConsumptionTaxRate = 10,
                ReserveQuantity = 0,
                DeliveryOrderQuantity = 0,
                DeliveredQuantity = 0,
                CompleteFlag = 0,
                Discount = 0,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private async Task SetupCustomer(string companyCode, int customerBranch)
        {
            var groupRepo = new CompanyGroupRepository(ConnectionString);
            var companyRepo = new CompanyRepository(ConnectionString);
            var customerRepo = new CustomerRepository(ConnectionString);

            // グループが既に存在しない場合のみ挿入
            var existingGroup = await groupRepo.FindByIdAsync("GRP1");
            if (existingGroup == null)
            {
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
            }

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

            await SetupDepartmentAndEmployee();

            var customer = new Customer
            {
                CustomerCode = companyCode,
                CustomerBranch = customerBranch,
                CustomerName = "テスト商事 本社",
                EmployeeCode = "EMP001",
                CustomerArType = 1,
                CustomerCloseDate1 = 31,
                CustomerPayMonths1 = 1,
                CustomerPayMethod1 = 1,
                CustomerCloseDate2 = 0,
                CustomerPayMonths2 = 0,
                CustomerPayMethod2 = 0,
                ArCode = companyCode,
                PayerCode = companyCode,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
            await customerRepo.InsertAsync(customer);
        }

        private async Task SetupDepartmentAndEmployee()
        {
            var deptRepo = new DepartmentRepository(ConnectionString);
            var empRepo = new EmployeeRepository(ConnectionString);

            // 部門が既に存在しない場合のみ挿入
            var existingDept = await deptRepo.FindByIdAsync("DEPT001", DateTime.Parse("2025-01-01"));
            if (existingDept == null)
            {
                var dept = new Department
                {
                    DepartmentCode = "DEPT001",
                    StartDate = DateTime.Parse("2025-01-01"),
                    EndDate = DateTime.Parse("9999-12-31"),
                    DepartmentName = "営業部",
                    OrganizationLevel = 1,
                    SlipInputFlag = 1,
                    CreatedAt = DateTime.Now,
                    CreatedBy = "admin",
                    UpdatedAt = DateTime.Now,
                    UpdatedBy = "admin"
                };
                await deptRepo.InsertAsync(dept);
            }

            // 社員が既に存在しない場合のみ挿入
            var existingEmp = await empRepo.FindByIdAsync("EMP001");
            if (existingEmp == null)
            {
                var emp = new Employee
                {
                    EmployeeCode = "EMP001",
                    EmployeeName = "山田太郎",
                    EmployeeNameKana = "ヤマダタロウ",
                    Gender = "M",
                    BirthDate = DateTime.Parse("1990-01-01"),
                    JoinDate = DateTime.Parse("2020-04-01"),
                    DepartmentCode = "DEPT001",
                    CreatedAt = DateTime.Now,
                    CreatedBy = "admin",
                    UpdatedAt = DateTime.Now,
                    UpdatedBy = "admin"
                };
                await empRepo.InsertAsync(emp);
            }
        }

        private async Task SetupProduct(string productCode, string categoryCode)
        {
            var catRepo = new ProductCategoryRepository(ConnectionString);
            var prodRepo = new ProductRepository(ConnectionString);

            // 商品分類が既に存在しない場合のみ挿入
            var existingCategory = await catRepo.FindByIdAsync(categoryCode);
            if (existingCategory == null)
            {
                var category = new ProductCategory
                {
                    ProductCategoryCode = categoryCode,
                    ProductCategoryName = "テストカテゴリ",
                    CreatedAt = DateTime.Now,
                    CreatedBy = "admin",
                    UpdatedAt = DateTime.Now,
                    UpdatedBy = "admin"
                };
                await catRepo.InsertAsync(category);
            }

            // 商品が既に存在しない場合のみ挿入
            var existingProduct = await prodRepo.FindByIdAsync(productCode);
            if (existingProduct == null)
            {
                var product = new Product
                {
                    ProductCode = productCode,
                    ProductFormalName = "テスト商品",
                    ProductAbbreviation = "テスト商品",
                    ProductNameKana = "テストショウヒン",
                    ProductType = "1",
                    ProductCategoryCode = categoryCode,
                    SellingPrice = 10000,
                    PurchasePrice = 8000,
                    CostOfSales = 8000,
                    TaxType = 1,
                    MiscellaneousType = 0,
                    InventoryManagementFlag = 1,
                    InventoryAllocationFlag = 1,
                    CreatedAt = DateTime.Now,
                    CreatedBy = "admin",
                    UpdatedAt = DateTime.Now,
                    UpdatedBy = "admin"
                };
                await prodRepo.InsertAsync(product);
            }
        }
    }
}
