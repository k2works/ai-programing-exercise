using FluentAssertions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;
using Xunit;

namespace SalesManagement.Tests.IntegrationTests
{
    /// <summary>
    /// 受注データのテスト
    /// </summary>
    public class OrderTests : DatabaseTestBase
    {
        [Fact]
        public async Task 受注を登録できる()
        {
            // Arrange
            await SetupCustomer("COMP001", 1);
            var orderRepo = new OrderRepository(ConnectionString);

            var order = CreateTestOrder("ORD001", "COMP001", 1);

            // Act
            await orderRepo.InsertAsync(order);

            // Assert
            var found = await orderRepo.FindByIdAsync("ORD001");
            found.Should().NotBeNull();
            found!.CustomerCode.Should().Be("COMP001");
        }

        [Fact]
        public async Task 受注と明細を一緒に登録できる()
        {
            // Arrange
            await SetupCustomer("COMP001", 1);
            await SetupProduct("PROD001", "CAT001");

            var orderRepo = new OrderRepository(ConnectionString);
            var detailRepo = new OrderDetailRepository(ConnectionString);

            // ヘッダ登録
            var order = CreateTestOrder("ORD001", "COMP001", 1);
            await orderRepo.InsertAsync(order);

            // 明細登録
            var detail1 = CreateTestOrderDetail("ORD001", 1, "PROD001", "商品A", 10000, 5);
            var detail2 = CreateTestOrderDetail("ORD001", 2, "PROD001", "商品A", 10000, 3);

            // Act
            await detailRepo.InsertAsync(detail1);
            await detailRepo.InsertAsync(detail2);

            // Assert
            var details = (await detailRepo.FindByOrderNoAsync("ORD001")).ToList();
            details.Should().HaveCount(2);
            details.Sum(d => d.Quantity).Should().Be(8);
        }

        [Fact]
        public async Task 受注を削除すると明細も削除される()
        {
            // Arrange
            await SetupOrderWithDetails("ORD001", "COMP001", 1, "PROD001");

            var orderRepo = new OrderRepository(ConnectionString);
            var detailRepo = new OrderDetailRepository(ConnectionString);

            // Act
            await orderRepo.DeleteAsync("ORD001");

            // Assert
            var order = await orderRepo.FindByIdAsync("ORD001");
            var details = await detailRepo.FindByOrderNoAsync("ORD001");

            order.Should().BeNull();
            details.Should().BeEmpty(); // CASCADE DELETE
        }

        [Fact]
        public async Task 顧客別に受注を検索できる()
        {
            // Arrange
            await SetupCustomer("COMP001", 1);
            await SetupCustomer("COMP002", 1);

            var orderRepo = new OrderRepository(ConnectionString);

            await orderRepo.InsertAsync(CreateTestOrder("ORD001", "COMP001", 1));
            await orderRepo.InsertAsync(CreateTestOrder("ORD002", "COMP001", 1));
            await orderRepo.InsertAsync(CreateTestOrder("ORD003", "COMP002", 1));

            // Act
            var orders = (await orderRepo.FindByCustomerCodeAsync("COMP001")).ToList();

            // Assert
            orders.Should().HaveCount(2);
            orders.All(o => o.CustomerCode == "COMP001").Should().BeTrue();
        }

        [Fact]
        public async Task 受注を更新できる()
        {
            // Arrange
            await SetupCustomer("COMP001", 1);
            var orderRepo = new OrderRepository(ConnectionString);

            var order = CreateTestOrder("ORD001", "COMP001", 1);
            await orderRepo.InsertAsync(order);

            // Act
            order.OrderAmount = 200000;
            order.SlipComment = "更新テスト";
            order.UpdatedAt = DateTime.Now;
            order.UpdatedBy = "updater";
            await orderRepo.UpdateAsync(order);

            // Assert
            var updated = await orderRepo.FindByIdAsync("ORD001");
            updated.Should().NotBeNull();
            updated!.OrderAmount.Should().Be(200000);
            updated.SlipComment.Should().Be("更新テスト");
        }

        // テストデータ作成ヘルパーメソッド
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
            // グループと取引先を作成
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

            // 部門と社員を作成
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
    }
}
