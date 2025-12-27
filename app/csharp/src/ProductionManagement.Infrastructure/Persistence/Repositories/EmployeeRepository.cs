using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Master;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 担当者リポジトリ実装
/// </summary>
public class EmployeeRepository : IEmployeeRepository
{
    private readonly string _connectionString;

    public EmployeeRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Employee employee)
    {
        const string sql = """
            INSERT INTO "担当者マスタ" (
                "担当者コード", "担当者名", "部門コード"
            ) VALUES (
                @EmployeeCode, @EmployeeName, @DepartmentCode
            )
            ON CONFLICT ("担当者コード") DO UPDATE SET
                "担当者名" = @EmployeeName,
                "部門コード" = @DepartmentCode,
                "更新日時" = CURRENT_TIMESTAMP
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            employee.EmployeeCode,
            employee.EmployeeName,
            employee.DepartmentCode
        });
    }

    public async Task<Employee?> FindByEmployeeCodeAsync(string employeeCode)
    {
        const string sql = """
            SELECT
                "担当者コード" AS EmployeeCode,
                "担当者名" AS EmployeeName,
                "部門コード" AS DepartmentCode,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "担当者マスタ"
            WHERE "担当者コード" = @EmployeeCode
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Employee>(sql, new { EmployeeCode = employeeCode });
    }

    public async Task<IReadOnlyList<Employee>> FindByDepartmentCodeAsync(string departmentCode)
    {
        const string sql = """
            SELECT
                "担当者コード" AS EmployeeCode,
                "担当者名" AS EmployeeName,
                "部門コード" AS DepartmentCode,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "担当者マスタ"
            WHERE "部門コード" = @DepartmentCode
            ORDER BY "担当者コード"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Employee>(sql, new { DepartmentCode = departmentCode });
        return result.ToList();
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync("""DELETE FROM "担当者マスタ" """);
    }
}
