using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Master;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 部門リポジトリ実装
/// </summary>
public class DepartmentRepository : IDepartmentRepository
{
    private readonly string _connectionString;

    public DepartmentRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Department department)
    {
        const string sql = """
            INSERT INTO "部門マスタ" (
                "部門コード", "部門名"
            ) VALUES (
                @DepartmentCode, @DepartmentName
            )
            ON CONFLICT ("部門コード") DO UPDATE SET
                "部門名" = @DepartmentName,
                "更新日時" = CURRENT_TIMESTAMP
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            department.DepartmentCode,
            department.DepartmentName
        });
    }

    public async Task<Department?> FindByDepartmentCodeAsync(string departmentCode)
    {
        const string sql = """
            SELECT
                "部門コード" AS DepartmentCode,
                "部門名" AS DepartmentName,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "部門マスタ"
            WHERE "部門コード" = @DepartmentCode
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Department>(sql, new { DepartmentCode = departmentCode });
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync("""DELETE FROM "部門マスタ" """);
    }
}
