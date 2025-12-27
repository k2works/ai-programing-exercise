using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 工数実績リポジトリ実装
/// </summary>
public class LaborHoursRepository : ILaborHoursRepository
{
    private readonly string _connectionString;

    public LaborHoursRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(LaborHours laborHours)
    {
        const string sql = """
            INSERT INTO "工数実績データ" (
                "工数実績番号", "作業指示番号", "品目コード", "工順", "工程コード",
                "部門コード", "担当者コード", "作業日", "工数", "備考", "作成者"
            ) VALUES (
                @LaborHoursNumber, @WorkOrderNumber, @ItemCode, @Sequence, @ProcessCode,
                @DepartmentCode, @EmployeeCode, @WorkDate, @Hours, @Remarks, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        laborHours.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            laborHours.LaborHoursNumber,
            laborHours.WorkOrderNumber,
            laborHours.ItemCode,
            laborHours.Sequence,
            laborHours.ProcessCode,
            laborHours.DepartmentCode,
            laborHours.EmployeeCode,
            laborHours.WorkDate,
            laborHours.Hours,
            laborHours.Remarks,
            laborHours.CreatedBy
        });
    }

    public async Task<LaborHours?> FindByLaborHoursNumberAsync(string laborHoursNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "工数実績番号" AS LaborHoursNumber,
                "作業指示番号" AS WorkOrderNumber,
                "品目コード" AS ItemCode,
                "工順" AS Sequence,
                "工程コード" AS ProcessCode,
                "部門コード" AS DepartmentCode,
                "担当者コード" AS EmployeeCode,
                "作業日" AS WorkDate,
                "工数" AS Hours,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "工数実績データ"
            WHERE "工数実績番号" = @LaborHoursNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<LaborHours>(sql, new { LaborHoursNumber = laborHoursNumber });
    }

    public async Task<IReadOnlyList<LaborHours>> FindByWorkOrderNumberAsync(string workOrderNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "工数実績番号" AS LaborHoursNumber,
                "作業指示番号" AS WorkOrderNumber,
                "品目コード" AS ItemCode,
                "工順" AS Sequence,
                "工程コード" AS ProcessCode,
                "部門コード" AS DepartmentCode,
                "担当者コード" AS EmployeeCode,
                "作業日" AS WorkDate,
                "工数" AS Hours,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "工数実績データ"
            WHERE "作業指示番号" = @WorkOrderNumber
            ORDER BY "工順", "作業日"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<LaborHours>(sql, new { WorkOrderNumber = workOrderNumber });
        return result.ToList();
    }

    public async Task<decimal> SumByWorkOrderAndSequenceAsync(string workOrderNumber, int sequence)
    {
        const string sql = """
            SELECT COALESCE(SUM("工数"), 0)
            FROM "工数実績データ"
            WHERE "作業指示番号" = @WorkOrderNumber AND "工順" = @Sequence
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.ExecuteScalarAsync<decimal>(sql, new { WorkOrderNumber = workOrderNumber, Sequence = sequence });
    }

    public async Task<decimal> SumByWorkOrderAsync(string workOrderNumber)
    {
        const string sql = """
            SELECT COALESCE(SUM("工数"), 0)
            FROM "工数実績データ"
            WHERE "作業指示番号" = @WorkOrderNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.ExecuteScalarAsync<decimal>(sql, new { WorkOrderNumber = workOrderNumber });
    }

    public async Task<decimal> SumByEmployeeAsync(string employeeCode, DateOnly startDate, DateOnly endDate)
    {
        const string sql = """
            SELECT COALESCE(SUM("工数"), 0)
            FROM "工数実績データ"
            WHERE "担当者コード" = @EmployeeCode
              AND "作業日" BETWEEN @StartDate AND @EndDate
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.ExecuteScalarAsync<decimal>(sql, new
        {
            EmployeeCode = employeeCode,
            StartDate = startDate,
            EndDate = endDate
        });
    }

    public async Task<string?> FindLatestLaborHoursNumberAsync(string prefix)
    {
        const string sql = """
            SELECT "工数実績番号" FROM "工数実績データ"
            WHERE "工数実績番号" LIKE @Prefix
            ORDER BY "工数実績番号" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<string>(sql, new { Prefix = prefix });
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync("""DELETE FROM "工数実績データ" """);
    }
}
