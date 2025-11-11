using FluentAssertions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;
using Xunit;

namespace SalesManagement.Tests.IntegrationTests
{
    /// <summary>
    /// 在庫データのテスト（5フィールド複合主キー）
    /// </summary>
    public class StockTests : DatabaseTestBase
    {
        [Fact]
        public async Task 在庫を登録できる()
        {
            // Arrange
            await SetupWarehouse("WH1");
            await SetupProduct("PROD001", "CAT001");

            var stockRepo = new StockRepository(ConnectionString);
            var stock = CreateTestStock("WH1", "PROD001", "LOT001", "1", "G", 100, 100);

            // Act
            await stockRepo.InsertAsync(stock);

            // Assert
            var found = await stockRepo.FindByIdAsync("WH1", "PROD001", "LOT001", "1", "G");
            found.Should().NotBeNull();
            found!.ActualQuantity.Should().Be(100);
            found.ValidQuantity.Should().Be(100);
        }

        [Fact]
        public async Task フィールド複合主キーで一意に識別できる()
        {
            // Arrange
            await SetupWarehouse("WH1");
            await SetupProduct("PROD001", "CAT001");

            var stockRepo = new StockRepository(ConnectionString);

            // 同じ倉庫・商品・ロットでも、在庫区分と良品区分が異なれば別レコード
            var stock1 = CreateTestStock("WH1", "PROD001", "LOT001", "1", "G", 100, 100); // 通常在庫・良品
            var stock2 = CreateTestStock("WH1", "PROD001", "LOT001", "1", "B", 10, 0);    // 通常在庫・不良品
            var stock3 = CreateTestStock("WH1", "PROD001", "LOT001", "2", "G", 50, 50);   // 預託在庫・良品

            // Act
            await stockRepo.InsertAsync(stock1);
            await stockRepo.InsertAsync(stock2);
            await stockRepo.InsertAsync(stock3);

            // Assert
            var found1 = await stockRepo.FindByIdAsync("WH1", "PROD001", "LOT001", "1", "G");
            var found2 = await stockRepo.FindByIdAsync("WH1", "PROD001", "LOT001", "1", "B");
            var found3 = await stockRepo.FindByIdAsync("WH1", "PROD001", "LOT001", "2", "G");

            found1.Should().NotBeNull();
            found2.Should().NotBeNull();
            found3.Should().NotBeNull();

            found1!.QualityType.Should().Be("G");
            found2!.QualityType.Should().Be("B");
            found3!.StockType.Should().Be("2");
        }

        [Fact]
        public async Task ロット番号で検索できる()
        {
            // Arrange
            await SetupWarehouse("WH1");
            await SetupWarehouse("WH2");
            await SetupProduct("PROD001", "CAT001");

            var stockRepo = new StockRepository(ConnectionString);

            // 同じロット番号の在庫を複数倉庫に登録
            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD001", "LOT001", "1", "G", 100, 100));
            await stockRepo.InsertAsync(CreateTestStock("WH2", "PROD001", "LOT001", "1", "G", 50, 50));
            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD001", "LOT002", "1", "G", 80, 80));

            // Act
            var stocks = (await stockRepo.FindByLotNoAsync("LOT001")).ToList();

            // Assert
            stocks.Should().HaveCount(2);
            stocks.All(s => s.LotNo == "LOT001").Should().BeTrue();
            stocks.Sum(s => s.ActualQuantity).Should().Be(150);
        }

        [Fact]
        public async Task 倉庫別に在庫を検索できる()
        {
            // Arrange
            await SetupWarehouse("WH1");
            await SetupProduct("PROD001", "CAT001");
            await SetupProduct("PROD002", "CAT001");

            var stockRepo = new StockRepository(ConnectionString);

            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD001", "LOT001", "1", "G", 100, 100));
            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD002", "LOT002", "1", "G", 50, 50));
            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD001", "LOT003", "1", "B", 10, 0));

            // Act
            var stocks = (await stockRepo.FindByWarehouseCodeAsync("WH1")).ToList();

            // Assert
            stocks.Should().HaveCount(3);
            stocks.All(s => s.WarehouseCode == "WH1").Should().BeTrue();
        }

        [Fact]
        public async Task 商品別に在庫を検索できる()
        {
            // Arrange
            await SetupWarehouse("WH1");
            await SetupWarehouse("WH2");
            await SetupProduct("PROD001", "CAT001");

            var stockRepo = new StockRepository(ConnectionString);

            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD001", "LOT001", "1", "G", 100, 100));
            await stockRepo.InsertAsync(CreateTestStock("WH2", "PROD001", "LOT002", "1", "G", 50, 50));
            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD001", "LOT001", "1", "B", 10, 0));

            // Act
            var stocks = (await stockRepo.FindByProductCodeAsync("PROD001")).ToList();

            // Assert
            stocks.Should().HaveCount(3);
            stocks.All(s => s.ProductCode == "PROD001").Should().BeTrue();
        }

        [Fact]
        public async Task 倉庫と商品で在庫合計を取得できる()
        {
            // Arrange
            await SetupWarehouse("WH1");
            await SetupProduct("PROD001", "CAT001");

            var stockRepo = new StockRepository(ConnectionString);

            // 良品のみが合計対象
            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD001", "LOT001", "1", "G", 100, 90));
            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD001", "LOT002", "1", "G", 50, 45));
            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD001", "LOT003", "1", "B", 10, 0)); // 不良品は除外

            // Act
            var (actualQty, validQty) = await stockRepo.GetStockSummaryAsync("WH1", "PROD001");

            // Assert
            actualQty.Should().Be(150); // 良品のみ合計
            validQty.Should().Be(135);  // 良品の有効在庫のみ合計
        }

        [Fact]
        public async Task 商品別の在庫合計を取得できる()
        {
            // Arrange
            await SetupWarehouse("WH1");
            await SetupWarehouse("WH2");
            await SetupProduct("PROD001", "CAT001");
            await SetupProduct("PROD002", "CAT001");

            var stockRepo = new StockRepository(ConnectionString);

            // PROD001の在庫
            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD001", "LOT001", "1", "G", 100, 90));
            await stockRepo.InsertAsync(CreateTestStock("WH2", "PROD001", "LOT002", "1", "G", 50, 45));

            // PROD002の在庫
            await stockRepo.InsertAsync(CreateTestStock("WH1", "PROD002", "LOT003", "1", "G", 80, 75));

            // Act
            var summary = (await stockRepo.GetStockSummaryByProductAsync()).ToList();

            // Assert
            summary.Should().HaveCount(2);

            var prod001 = summary.First(s => s.ProductCode == "PROD001");
            prod001.ActualQuantity.Should().Be(150);
            prod001.ValidQuantity.Should().Be(135);

            var prod002 = summary.First(s => s.ProductCode == "PROD002");
            prod002.ActualQuantity.Should().Be(80);
            prod002.ValidQuantity.Should().Be(75);
        }

        [Fact]
        public async Task 在庫数量を更新できる()
        {
            // Arrange
            await SetupWarehouse("WH1");
            await SetupProduct("PROD001", "CAT001");

            var stockRepo = new StockRepository(ConnectionString);
            var stock = CreateTestStock("WH1", "PROD001", "LOT001", "1", "G", 100, 100);
            await stockRepo.InsertAsync(stock);

            // Act
            stock.ActualQuantity = 80;   // 出荷により実在庫減少
            stock.ValidQuantity = 70;     // 引当により有効在庫減少
            stock.LastDeliveryDate = DateTime.Now;
            stock.UpdatedAt = DateTime.Now;
            stock.UpdatedBy = "updater";
            await stockRepo.UpdateAsync(stock);

            // Assert
            var updated = await stockRepo.FindByIdAsync("WH1", "PROD001", "LOT001", "1", "G");
            updated.Should().NotBeNull();
            updated!.ActualQuantity.Should().Be(80);
            updated.ValidQuantity.Should().Be(70);
            updated.LastDeliveryDate.Should().NotBeNull();
        }

        [Fact]
        public async Task 在庫を削除できる()
        {
            // Arrange
            await SetupWarehouse("WH1");
            await SetupProduct("PROD001", "CAT001");

            var stockRepo = new StockRepository(ConnectionString);
            var stock = CreateTestStock("WH1", "PROD001", "LOT001", "1", "G", 100, 100);
            await stockRepo.InsertAsync(stock);

            // Act
            await stockRepo.DeleteAsync("WH1", "PROD001", "LOT001", "1", "G");

            // Assert
            var deleted = await stockRepo.FindByIdAsync("WH1", "PROD001", "LOT001", "1", "G");
            deleted.Should().BeNull();
        }

        // テストデータ作成ヘルパーメソッド
        private static Stock CreateTestStock(string warehouseCode, string productCode, string lotNo,
            string stockType, string qualityType, int actualQuantity, int validQuantity)
        {
            return new Stock
            {
                WarehouseCode = warehouseCode,
                ProductCode = productCode,
                LotNo = lotNo,
                StockType = stockType,
                QualityType = qualityType,
                ActualQuantity = actualQuantity,
                ValidQuantity = validQuantity,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private async Task SetupWarehouse(string warehouseCode)
        {
            var warehouseRepo = new WarehouseRepository(ConnectionString);
            await SetupDepartmentAndEmployee();

            var existingWarehouse = await warehouseRepo.FindByIdAsync(warehouseCode);
            if (existingWarehouse == null)
            {
                var warehouse = new Warehouse
                {
                    WarehouseCode = warehouseCode,
                    WarehouseName = $"テスト倉庫{warehouseCode}",
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
                    ProductFormalName = $"テスト商品{productCode}",
                    ProductAbbreviation = $"テスト商品{productCode}",
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
    }
}
