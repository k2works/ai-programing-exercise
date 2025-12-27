using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 工程表リポジトリ実装
/// </summary>
public class RoutingRepository : IRoutingRepository
{
    private readonly string _connectionString;

    public RoutingRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Routing routing)
    {
        const string sql = """
            INSERT INTO "工程表" (
                "品目コード", "工順", "工程コード"
            ) VALUES (
                @ItemCode, @Sequence, @ProcessCode
            )
            ON CONFLICT ("品目コード", "工順") DO UPDATE SET
                "工程コード" = @ProcessCode,
                "更新日時" = CURRENT_TIMESTAMP
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            routing.ItemCode,
            routing.Sequence,
            routing.ProcessCode
        });
    }

    public async Task<IReadOnlyList<Routing>> FindByItemCodeAsync(string itemCode)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "品目コード" AS ItemCode,
                "工順" AS Sequence,
                "工程コード" AS ProcessCode,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "工程表"
            WHERE "品目コード" = @ItemCode
            ORDER BY "工順"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Routing>(sql, new { ItemCode = itemCode });
        return result.ToList();
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync("""DELETE FROM "工程表" """);
    }
}
