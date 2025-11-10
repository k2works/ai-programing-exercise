using FluentAssertions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;
using Xunit;

namespace SalesManagement.Tests.IntegrationTests
{
    /// <summary>
    /// 部門マスタのテスト（Dapper版）
    /// </summary>
    public class DepartmentTests : DatabaseTestBase
    {
        [Fact]
        public async Task 部門を登録できる()
        {
            // Arrange
            var repository = new DepartmentRepository(ConnectionString);
            var department = new Department
            {
                DepartmentCode = "11101",
                StartDate = new DateTime(2021, 1, 1),
                EndDate = new DateTime(2100, 12, 31),
                DepartmentName = "新規部署",
                OrganizationLevel = 1,
                DepartmentPath = "D0001",
                LowestLevelFlag = 1,
                SlipInputFlag = 0,
                CreatedAt = new DateTime(2021, 1, 1, 0, 0, 0),
                CreatedBy = "admin",
                UpdatedAt = new DateTime(2021, 1, 1, 0, 0, 0),
                UpdatedBy = "admin"
            };

            // Act
            await repository.InsertAsync(department);

            // Assert
            var found = await repository.FindByIdAsync("11101", new DateTime(2021, 1, 1));
            found.Should().NotBeNull();
            found!.DepartmentName.Should().Be("新規部署");
            found.OrganizationLevel.Should().Be(1);
        }

        [Fact]
        public async Task 部門を更新できる()
        {
            // Arrange
            var repository = new DepartmentRepository(ConnectionString);
            var department = CreateTestDepartment("11101", "新規部署");
            await repository.InsertAsync(department);

            // Act
            department.DepartmentName = "更新部署";
            department.UpdatedAt = DateTime.Now;
            department.UpdatedBy = "updater";
            await repository.UpdateAsync(department);

            // Assert
            var updated = await repository.FindByIdAsync("11101", new DateTime(2021, 1, 1));
            updated.Should().NotBeNull();
            updated!.DepartmentName.Should().Be("更新部署");
            updated.UpdatedBy.Should().Be("updater");
        }

        [Fact]
        public async Task 部門を削除できる()
        {
            // Arrange
            var repository = new DepartmentRepository(ConnectionString);
            var department = CreateTestDepartment("11101", "削除対象部署");
            await repository.InsertAsync(department);

            // Act
            await repository.DeleteAsync("11101", new DateTime(2021, 1, 1));

            // Assert
            var deleted = await repository.FindByIdAsync("11101", new DateTime(2021, 1, 1));
            deleted.Should().BeNull();
        }

        [Fact]
        public async Task 複数の部門を登録して階層構造を確認できる()
        {
            // Arrange
            var repository = new DepartmentRepository(ConnectionString);

            // 親部門（第1階層）
            var parent = CreateTestDepartment("10000", "本社");
            parent.OrganizationLevel = 1;
            parent.DepartmentPath = "10000";
            parent.LowestLevelFlag = 0;
            await repository.InsertAsync(parent);

            // 子部門（第2階層）
            var child = CreateTestDepartment("11000", "営業本部");
            child.OrganizationLevel = 2;
            child.DepartmentPath = "10000/11000";
            child.LowestLevelFlag = 0;
            await repository.InsertAsync(child);

            // 孫部門（第3階層）
            var grandChild = CreateTestDepartment("11101", "営業一課");
            grandChild.OrganizationLevel = 3;
            grandChild.DepartmentPath = "10000/11000/11101";
            grandChild.LowestLevelFlag = 1;
            grandChild.SlipInputFlag = 1;
            await repository.InsertAsync(grandChild);

            // Act
            var departments = (await repository.FindAllAsync()).ToList();

            // Assert
            departments.Should().HaveCount(3);
            departments[0].DepartmentName.Should().Be("本社");
            departments[0].OrganizationLevel.Should().Be(1);
            departments[1].DepartmentName.Should().Be("営業本部");
            departments[1].OrganizationLevel.Should().Be(2);
            departments[2].DepartmentName.Should().Be("営業一課");
            departments[2].OrganizationLevel.Should().Be(3);
        }

        // テストデータ作成ヘルパーメソッド
        private static Department CreateTestDepartment(string code, string name)
        {
            return new Department
            {
                DepartmentCode = code,
                StartDate = new DateTime(2021, 1, 1),
                EndDate = new DateTime(2100, 12, 31),
                DepartmentName = name,
                OrganizationLevel = 1,
                DepartmentPath = code,
                LowestLevelFlag = 1,
                SlipInputFlag = 0,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }
    }
}
