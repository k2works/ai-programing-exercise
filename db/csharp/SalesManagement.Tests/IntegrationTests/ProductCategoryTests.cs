using FluentAssertions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;
using Xunit;

namespace SalesManagement.Tests.IntegrationTests
{
    /// <summary>
    /// 商品分類マスタのテスト
    /// </summary>
    public class ProductCategoryTests : DatabaseTestBase
    {
        [Fact]
        public async Task 商品分類を登録できる()
        {
            // Arrange
            var repository = new ProductCategoryRepository(ConnectionString);
            var category = CreateTestCategory("CAT001", "電子機器", 1, "CAT001");

            // Act
            await repository.InsertAsync(category);

            // Assert
            var found = await repository.FindByIdAsync("CAT001");
            found.Should().NotBeNull();
            found!.ProductCategoryName.Should().Be("電子機器");
        }

        [Fact]
        public async Task 階層構造の商品分類を登録できる()
        {
            // Arrange
            var repository = new ProductCategoryRepository(ConnectionString);

            // 第1階層
            var parent = CreateTestCategory("CAT001", "電子機器", 1, "CAT001");
            parent.LowestLevelFlag = 0;
            await repository.InsertAsync(parent);

            // 第2階層
            var child = CreateTestCategory("CAT00101", "パソコン", 2, "CAT001/CAT00101");
            child.LowestLevelFlag = 0;
            await repository.InsertAsync(child);

            // 第3階層
            var grandChild = CreateTestCategory("CAT0010101", "ノートPC", 3, "CAT001/CAT00101/CAT0010101");
            grandChild.LowestLevelFlag = 1;
            await repository.InsertAsync(grandChild);

            // Act
            var categories = (await repository.FindAllAsync()).ToList();

            // Assert
            categories.Should().HaveCount(3);
            categories[0].ProductCategoryLevel.Should().Be(1);
            categories[1].ProductCategoryLevel.Should().Be(2);
            categories[2].ProductCategoryLevel.Should().Be(3);
        }

        [Fact]
        public async Task 階層パスで配下の商品分類を検索できる()
        {
            // Arrange
            var repository = new ProductCategoryRepository(ConnectionString);

            await repository.InsertAsync(CreateTestCategory("CAT001", "電子機器", 1, "CAT001"));
            await repository.InsertAsync(CreateTestCategory("CAT00101", "パソコン", 2, "CAT001/CAT00101"));
            await repository.InsertAsync(CreateTestCategory("CAT0010101", "ノートPC", 3, "CAT001/CAT00101/CAT0010101"));
            await repository.InsertAsync(CreateTestCategory("CAT002", "家具", 1, "CAT002"));

            // Act
            var underCat001 = (await repository.FindByPathPrefixAsync("CAT001")).ToList();

            // Assert
            underCat001.Should().HaveCount(3);
        }

        [Fact]
        public async Task 商品分類を更新できる()
        {
            // Arrange
            var repository = new ProductCategoryRepository(ConnectionString);
            var category = CreateTestCategory("CAT001", "電子機器", 1, "CAT001");
            await repository.InsertAsync(category);

            // Act
            category.ProductCategoryName = "情報機器";
            category.UpdatedAt = DateTime.Now;
            category.UpdatedBy = "updater";
            await repository.UpdateAsync(category);

            // Assert
            var updated = await repository.FindByIdAsync("CAT001");
            updated.Should().NotBeNull();
            updated!.ProductCategoryName.Should().Be("情報機器");
            updated.UpdatedBy.Should().Be("updater");
        }

        [Fact]
        public async Task 商品分類を削除できる()
        {
            // Arrange
            var repository = new ProductCategoryRepository(ConnectionString);
            var category = CreateTestCategory("CAT001", "電子機器", 1, "CAT001");
            await repository.InsertAsync(category);

            // Act
            await repository.DeleteAsync("CAT001");

            // Assert
            var deleted = await repository.FindByIdAsync("CAT001");
            deleted.Should().BeNull();
        }

        // テストデータ作成ヘルパーメソッド
        private static ProductCategory CreateTestCategory(string code, string name, int level, string path)
        {
            return new ProductCategory
            {
                ProductCategoryCode = code,
                ProductCategoryName = name,
                ProductCategoryLevel = level,
                ProductCategoryPath = path,
                LowestLevelFlag = 1,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }
    }
}
