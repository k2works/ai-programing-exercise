using FluentAssertions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;
using Xunit;

namespace SalesManagement.Tests.IntegrationTests
{
    /// <summary>
    /// 発注データのテスト
    /// </summary>
    public class PurchaseOrderTests : DatabaseTestBase
    {
        [Fact]
        public async Task 発注を登録できる()
        {
            // Arrange
            await SetupOrderAndSupplier("ORD001", "SUP001");
            await SetupWarehouse("WH1");

            var poRepo = new PurchaseOrderRepository(ConnectionString);
            var po = CreateTestPurchaseOrder("PO001", "ORD001", "SUP001", 1);

            // Act
            await poRepo.InsertAsync(po);

            // Assert
            var found = await poRepo.FindByIdAsync("PO001");
            found.Should().NotBeNull();
            found!.OrderNo.Should().Be("ORD001");
            found.SupplierCode.Should().Be("SUP001");
        }

        [Fact]
        public async Task 発注と明細を一緒に登録できる()
        {
            // Arrange
            await SetupOrderAndSupplier("ORD001", "SUP001");
            await SetupWarehouse("WH1");
            await SetupProduct("PROD001", "CAT001");

            var poRepo = new PurchaseOrderRepository(ConnectionString);
            var poDetailRepo = new PurchaseOrderDetailRepository(ConnectionString);

            // ヘッダ登録
            var po = CreateTestPurchaseOrder("PO001", "ORD001", "SUP001", 1);
            await poRepo.InsertAsync(po);

            // 明細登録
            var detail1 = CreateTestPurchaseOrderDetail("PO001", 1, "PROD001", "商品A", 8000, 5);
            var detail2 = CreateTestPurchaseOrderDetail("PO001", 2, "PROD001", "商品A", 8000, 3);

            // Act
            await poDetailRepo.InsertAsync(detail1);
            await poDetailRepo.InsertAsync(detail2);

            // Assert
            var details = (await poDetailRepo.FindByPoNoAsync("PO001")).ToList();
            details.Should().HaveCount(2);
            details.Sum(d => d.Quantity).Should().Be(8);
        }

        [Fact]
        public async Task 発注を削除すると明細も削除される()
        {
            // Arrange
            await SetupPurchaseOrderWithDetails("PO001", "ORD001", "SUP001", "PROD001");

            var poRepo = new PurchaseOrderRepository(ConnectionString);
            var poDetailRepo = new PurchaseOrderDetailRepository(ConnectionString);

            // Act
            await poRepo.DeleteAsync("PO001");

            // Assert
            var po = await poRepo.FindByIdAsync("PO001");
            var details = await poDetailRepo.FindByPoNoAsync("PO001");

            po.Should().BeNull();
            details.Should().BeEmpty(); // CASCADE DELETE
        }

        [Fact]
        public async Task 受注番号で発注を検索できる()
        {
            // Arrange
            await SetupOrderAndSupplier("ORD001", "SUP001");
            await SetupOrderAndSupplier("ORD002", "SUP001");
            await SetupWarehouse("WH1");

            var poRepo = new PurchaseOrderRepository(ConnectionString);

            await poRepo.InsertAsync(CreateTestPurchaseOrder("PO001", "ORD001", "SUP001", 1));
            await poRepo.InsertAsync(CreateTestPurchaseOrder("PO002", "ORD001", "SUP001", 1));
            await poRepo.InsertAsync(CreateTestPurchaseOrder("PO003", "ORD002", "SUP001", 1));

            // Act
            var pos = (await poRepo.FindByOrderNoAsync("ORD001")).ToList();

            // Assert
            pos.Should().HaveCount(2);
            pos.All(p => p.OrderNo == "ORD001").Should().BeTrue();
        }

        [Fact]
        public async Task 仕入先別に発注を検索できる()
        {
            // Arrange
            await SetupOrderAndSupplier("ORD001", "SUP001");
            await SetupSupplier("SUP002", 1);
            await SetupWarehouse("WH1");

            var poRepo = new PurchaseOrderRepository(ConnectionString);

            await poRepo.InsertAsync(CreateTestPurchaseOrder("PO001", "ORD001", "SUP001", 1));
            await poRepo.InsertAsync(CreateTestPurchaseOrder("PO002", "ORD001", "SUP001", 1));
            await poRepo.InsertAsync(CreateTestPurchaseOrder("PO003", "ORD001", "SUP002", 1));

            // Act
            var pos = (await poRepo.FindBySupplierAsync("SUP001", 1)).ToList();

            // Assert
            pos.Should().HaveCount(2);
            pos.All(p => p.SupplierCode == "SUP001" && p.SupplierBranch == 1).Should().BeTrue();
        }

        [Fact]
        public async Task 発注を更新できる()
        {
            // Arrange
            await SetupOrderAndSupplier("ORD001", "SUP001");
            await SetupWarehouse("WH1");

            var poRepo = new PurchaseOrderRepository(ConnectionString);
            var po = CreateTestPurchaseOrder("PO001", "ORD001", "SUP001", 1);
            await poRepo.InsertAsync(po);

            // Act
            po.PoAmount = 200000;
            po.SlipComment = "更新テスト";
            po.UpdatedAt = DateTime.Now;
            po.UpdatedBy = "updater";
            await poRepo.UpdateAsync(po);

            // Assert
            var updated = await poRepo.FindByIdAsync("PO001");
            updated.Should().NotBeNull();
            updated!.PoAmount.Should().Be(200000);
            updated.SlipComment.Should().Be("更新テスト");
        }

        // テストデータ作成ヘルパーメソッド
        private static PurchaseOrder CreateTestPurchaseOrder(string poNo, string orderNo,
            string supplierCode, int supplierBranch)
        {
            return new PurchaseOrder
            {
                PoNo = poNo,
                PoDate = DateTime.Now,
                OrderNo = orderNo,
                SupplierCode = supplierCode,
                SupplierBranch = supplierBranch,
                EmployeeCode = "EMP001",
                WarehouseCode = "WH1",
                PoAmount = 100000,
                ConsumptionTax = 10000,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private static PurchaseOrderDetail CreateTestPurchaseOrderDetail(string poNo, int poRowNo,
            string productCode, string productName, int unitPrice, int quantity)
        {
            return new PurchaseOrderDetail
            {
                PoNo = poNo,
                PoRowNo = poRowNo,
                ProductCode = productCode,
                ProductName = productName,
                UnitPrice = unitPrice,
                Quantity = quantity,
                ConsumptionTaxRate = 10,
                ReceivedQuantity = 0,
                Discount = 0,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private async Task SetupOrderAndSupplier(string orderNo, string supplierCode)
        {
            await SetupCustomer("COMP001", 1);
            await SetupSupplier(supplierCode, 1);

            var orderRepo = new OrderRepository(ConnectionString);
            var order = new Order
            {
                OrderNo = orderNo,
                OrderDate = DateTime.Now,
                DepartmentCode = "DEPT001",
                StartDate = DateTime.Parse("2025-01-01"),
                CustomerCode = "COMP001",
                CustomerBranch = 1,
                EmployeeCode = "EMP001",
                WarehouseCode = "WH1",
                OrderAmount = 100000,
                ConsumptionTax = 10000,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
            await orderRepo.InsertAsync(order);
        }

        private async Task SetupSupplier(string supplierCode, int supplierBranch)
        {
            var groupRepo = new CompanyGroupRepository(ConnectionString);
            var companyRepo = new CompanyRepository(ConnectionString);
            var supplierRepo = new SupplierRepository(ConnectionString);

            // グループが既に存在しない場合のみ挿入
            var existingGroup = await groupRepo.FindByIdAsync("GRP1");
            if (existingGroup == null)
            {
                var group = new CompanyGroup
                {
                    CompanyGroupCode = "GRP1",
                    CompanyGroupName = "仕入先グループ",
                    CreatedAt = DateTime.Now,
                    CreatedBy = "admin",
                    UpdatedAt = DateTime.Now,
                    UpdatedBy = "admin"
                };
                await groupRepo.InsertAsync(group);
            }

            // 取引先が既に存在しない場合のみ挿入
            var existingCompany = await companyRepo.FindByIdAsync(supplierCode);
            if (existingCompany == null)
            {
                var company = new Company
                {
                    CompanyCode = supplierCode,
                    CompanyName = "株式会社テスト仕入先",
                    CompanyNameKana = "テストシイレサキ",
                    SupplierType = 1,
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

            // 仕入先が既に存在しない場合のみ挿入
            var existingSupplier = await supplierRepo.FindByIdAsync(supplierCode, supplierBranch);
            if (existingSupplier == null)
            {
                var supplier = new Supplier
                {
                    SupplierCode = supplierCode,
                    SupplierBranch = supplierBranch,
                    SupplierName = "テスト仕入先 本社",
                    SupplierCloseDate = 31,
                    SupplierPayMonths = 1,
                    SupplierPayMethod = 1,
                    CreatedAt = DateTime.Now,
                    CreatedBy = "admin",
                    UpdatedAt = DateTime.Now,
                    UpdatedBy = "admin"
                };
                await supplierRepo.InsertAsync(supplier);
            }
        }

        private async Task SetupWarehouse(string warehouseCode)
        {
            var warehouseRepo = new WarehouseRepository(ConnectionString);

            // 倉庫が既に存在しない場合のみ挿入
            var existingWarehouse = await warehouseRepo.FindByIdAsync(warehouseCode);
            if (existingWarehouse == null)
            {
                var warehouse = new Warehouse
                {
                    WarehouseCode = warehouseCode,
                    WarehouseName = "テスト倉庫",
                    WarehouseType = 1,
                    Address = "東京都",
                    PhoneNumber = "03-1234-5678",
                    ManagerCode = "EMP001",
                    CreatedAt = DateTime.Now,
                    CreatedBy = "admin",
                    UpdatedAt = DateTime.Now,
                    UpdatedBy = "admin"
                };
                await warehouseRepo.InsertAsync(warehouse);
            }
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

            // 取引先が既に存在しない場合のみ挿入
            var existingCompany = await companyRepo.FindByIdAsync(companyCode);
            if (existingCompany == null)
            {
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

            await SetupDepartmentAndEmployee();

            // 顧客が既に存在しない場合のみ挿入
            var existingCustomer = await customerRepo.FindByIdAsync(companyCode, customerBranch);
            if (existingCustomer == null)
            {
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
        }

        private async Task SetupDepartmentAndEmployee()
        {
            var deptRepo = new DepartmentRepository(ConnectionString);
            var empRepo = new EmployeeRepository(ConnectionString);

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

        private async Task SetupPurchaseOrderWithDetails(string poNo, string orderNo, string supplierCode, string productCode)
        {
            await SetupOrderAndSupplier(orderNo, supplierCode);
            await SetupWarehouse("WH1");
            await SetupProduct(productCode, "CAT001");

            var poRepo = new PurchaseOrderRepository(ConnectionString);
            var poDetailRepo = new PurchaseOrderDetailRepository(ConnectionString);

            var po = CreateTestPurchaseOrder(poNo, orderNo, supplierCode, 1);
            await poRepo.InsertAsync(po);

            var detail = CreateTestPurchaseOrderDetail(poNo, 1, productCode, "テスト商品", 8000, 5);
            await poDetailRepo.InsertAsync(detail);
        }
    }
}
