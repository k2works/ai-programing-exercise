using FluentAssertions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;
using Xunit;

namespace SalesManagement.Tests.IntegrationTests
{
    /// <summary>
    /// 商品マスタのテスト
    /// </summary>
    public class ProductTests : DatabaseTestBase
    {
        [Fact]
        public async Task 商品を登録できる()
        {
            // Arrange
            var categoryRepo = new ProductCategoryRepository(ConnectionString);
            var productRepo = new ProductRepository(ConnectionString);

            // 前提：商品分類が存在
            var category = CreateTestCategory("CAT001", "電子機器");
            await categoryRepo.InsertAsync(category);

            var product = CreateTestProduct("PROD001", "ノートパソコン A型", "CAT001");

            // Act
            await productRepo.InsertAsync(product);

            // Assert
            var found = await productRepo.FindByIdAsync("PROD001");
            found.Should().NotBeNull();
            found!.ProductFormalName.Should().Be("ノートパソコン A型");
            found.SellingPrice.Should().Be(150000);
        }

        [Fact]
        public async Task 商品分類コードで商品を検索できる()
        {
            // Arrange
            var categoryRepo = new ProductCategoryRepository(ConnectionString);
            var productRepo = new ProductRepository(ConnectionString);

            var category = CreateTestCategory("CAT001", "電子機器");
            await categoryRepo.InsertAsync(category);

            await productRepo.InsertAsync(CreateTestProduct("PROD001", "ノートPC A型", "CAT001"));
            await productRepo.InsertAsync(CreateTestProduct("PROD002", "ノートPC B型", "CAT001"));

            // Act
            var products = (await productRepo.FindByCategoryAsync("CAT001")).ToList();

            // Assert
            products.Should().HaveCount(2);
        }

        [Fact]
        public async Task 商品を更新できる()
        {
            // Arrange
            var categoryRepo = new ProductCategoryRepository(ConnectionString);
            var productRepo = new ProductRepository(ConnectionString);

            var category = CreateTestCategory("CAT001", "電子機器");
            await categoryRepo.InsertAsync(category);

            var product = CreateTestProduct("PROD001", "ノートPC A型", "CAT001");
            await productRepo.InsertAsync(product);

            // Act
            product.ProductFormalName = "ノートPC A型 改訂版";
            product.SellingPrice = 160000;
            product.UpdatedAt = DateTime.Now;
            product.UpdatedBy = "updater";
            await productRepo.UpdateAsync(product);

            // Assert
            var updated = await productRepo.FindByIdAsync("PROD001");
            updated.Should().NotBeNull();
            updated!.ProductFormalName.Should().Be("ノートPC A型 改訂版");
            updated.SellingPrice.Should().Be(160000);
        }

        [Fact]
        public async Task 商品を削除できる()
        {
            // Arrange
            var categoryRepo = new ProductCategoryRepository(ConnectionString);
            var productRepo = new ProductRepository(ConnectionString);

            var category = CreateTestCategory("CAT001", "電子機器");
            await categoryRepo.InsertAsync(category);

            var product = CreateTestProduct("PROD001", "ノートPC A型", "CAT001");
            await productRepo.InsertAsync(product);

            // Act
            await productRepo.DeleteAsync("PROD001");

            // Assert
            var deleted = await productRepo.FindByIdAsync("PROD001");
            deleted.Should().BeNull();
        }

        // テストデータ作成ヘルパーメソッド
        private static ProductCategory CreateTestCategory(string code, string name)
        {
            return new ProductCategory
            {
                ProductCategoryCode = code,
                ProductCategoryName = name,
                ProductCategoryLevel = 1,
                ProductCategoryPath = code,
                LowestLevelFlag = 1,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private static Product CreateTestProduct(string code, string name, string categoryCode)
        {
            return new Product
            {
                ProductCode = code,
                ProductFormalName = name,
                ProductAbbreviation = name,
                ProductNameKana = "ノートパソコン",
                ProductType = "PRODUCT",
                ModelNumber = "MODEL-001",
                SellingPrice = 150000,
                PurchasePrice = 100000,
                CostOfSales = 100000,
                TaxType = 1,
                ProductCategoryCode = categoryCode,
                MiscellaneousType = 0,
                InventoryManagementFlag = 1,
                InventoryAllocationFlag = 0,
                SupplierCode = null,
                SupplierBranch = null,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }
    }
}
