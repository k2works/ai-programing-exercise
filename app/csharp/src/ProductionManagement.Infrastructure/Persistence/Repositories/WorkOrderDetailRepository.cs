using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 作業指示明細リポジトリ実装
/// </summary>
public class WorkOrderDetailRepository : IWorkOrderDetailRepository
{
    private readonly string _connectionString;

    public WorkOrderDetailRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(WorkOrderDetail detail)
    {
        const string sql = """
            INSERT INTO "作業指示明細データ" (
                "作業指示番号", "工順", "工程コード"
            ) VALUES (
                @WorkOrderNumber, @Sequence, @ProcessCode
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        detail.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            detail.WorkOrderNumber,
            detail.Sequence,
            detail.ProcessCode
        });
    }

    public async Task<IReadOnlyList<WorkOrderDetail>> FindByWorkOrderNumberAsync(string workOrderNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "作業指示番号" AS WorkOrderNumber,
                "工順" AS Sequence,
                "工程コード" AS ProcessCode,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "作業指示明細データ"
            WHERE "作業指示番号" = @WorkOrderNumber
            ORDER BY "工順"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<WorkOrderDetail>(sql, new { WorkOrderNumber = workOrderNumber });
        return result.ToList();
    }

    public async Task<WorkOrderDetail?> FindByWorkOrderAndSequenceAsync(string workOrderNumber, int sequence)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "作業指示番号" AS WorkOrderNumber,
                "工順" AS Sequence,
                "工程コード" AS ProcessCode,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "作業指示明細データ"
            WHERE "作業指示番号" = @WorkOrderNumber AND "工順" = @Sequence
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<WorkOrderDetail>(sql, new { WorkOrderNumber = workOrderNumber, Sequence = sequence });
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync("""DELETE FROM "作業指示明細データ" """);
    }
}
