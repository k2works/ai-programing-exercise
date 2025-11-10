using FluentAssertions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;
using Xunit;

namespace SalesManagement.Tests.IntegrationTests
{
    /// <summary>
    /// 社員マスタのテスト（Dapper版）
    /// </summary>
    public class EmployeeTests : DatabaseTestBase
    {
        [Fact]
        public async Task 社員を登録できる()
        {
            // Arrange
            var departmentRepo = new DepartmentRepository(ConnectionString);
            var employeeRepo = new EmployeeRepository(ConnectionString);

            var department = CreateTestDepartment("10000", "本社");
            await departmentRepo.InsertAsync(department);

            var employee = CreateTestEmployee("EMP001", "山田太郎", "10000");

            // Act
            await employeeRepo.InsertAsync(employee);

            // Assert
            var found = await employeeRepo.FindByIdAsync("EMP001");
            found.Should().NotBeNull();
            found!.EmployeeName.Should().Be("山田太郎");
            found.DepartmentCode.Should().Be("10000");
        }

        [Fact]
        public async Task 社員を更新できる()
        {
            // Arrange
            var departmentRepo = new DepartmentRepository(ConnectionString);
            var employeeRepo = new EmployeeRepository(ConnectionString);

            var department = CreateTestDepartment("10000", "本社");
            await departmentRepo.InsertAsync(department);

            var employee = CreateTestEmployee("EMP001", "山田太郎", "10000");
            await employeeRepo.InsertAsync(employee);

            // Act
            employee.EmployeeName = "山田次郎";
            employee.UpdatedAt = DateTime.Now;
            employee.UpdatedBy = "updater";
            await employeeRepo.UpdateAsync(employee);

            // Assert
            var updated = await employeeRepo.FindByIdAsync("EMP001");
            updated.Should().NotBeNull();
            updated!.EmployeeName.Should().Be("山田次郎");
        }

        [Fact]
        public async Task 社員を削除できる()
        {
            // Arrange
            var departmentRepo = new DepartmentRepository(ConnectionString);
            var employeeRepo = new EmployeeRepository(ConnectionString);

            var department = CreateTestDepartment("10000", "本社");
            await departmentRepo.InsertAsync(department);

            var employee = CreateTestEmployee("EMP001", "山田太郎", "10000");
            await employeeRepo.InsertAsync(employee);

            // Act
            await employeeRepo.DeleteAsync("EMP001");

            // Assert
            var deleted = await employeeRepo.FindByIdAsync("EMP001");
            deleted.Should().BeNull();
        }

        [Fact]
        public async Task 部門に所属する社員を取得できる()
        {
            // Arrange
            var departmentRepo = new DepartmentRepository(ConnectionString);
            var employeeRepo = new EmployeeRepository(ConnectionString);

            var department = CreateTestDepartment("10000", "本社");
            await departmentRepo.InsertAsync(department);

            var employee1 = CreateTestEmployee("EMP001", "山田太郎", "10000");
            var employee2 = CreateTestEmployee("EMP002", "佐藤花子", "10000");
            await employeeRepo.InsertAsync(employee1);
            await employeeRepo.InsertAsync(employee2);

            // Act
            var employees = (await employeeRepo.FindEmployeesWithDepartmentAsync("10000")).ToList();

            // Assert
            employees.Should().HaveCount(2);
            ((string)employees[0].社員名).Should().Be("山田太郎");
            ((string)employees[0].部門名).Should().Be("本社");
            ((string)employees[1].社員名).Should().Be("佐藤花子");
            ((string)employees[1].部門名).Should().Be("本社");
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
                SlipInputFlag = 1,
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }

        private static Employee CreateTestEmployee(string code, string name, string deptCode)
        {
            return new Employee
            {
                EmployeeCode = code,
                EmployeeName = name,
                EmployeeNameKana = "ヤマダタロウ",
                Gender = "M",
                BirthDate = new DateTime(1990, 1, 1),
                JoinDate = new DateTime(2015, 4, 1),
                DepartmentCode = deptCode,
                PositionCode = "POS001",
                CreatedAt = DateTime.Now,
                CreatedBy = "admin",
                UpdatedAt = DateTime.Now,
                UpdatedBy = "admin"
            };
        }
    }
}
