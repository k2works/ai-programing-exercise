using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 工程マスタリポジトリ実装
/// </summary>
public class ProcessRepository : IProcessRepository
{
    private readonly string _connectionString;

    public ProcessRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Process process)
    {
        const string sql = """
            INSERT INTO "工程マスタ" (
                "工程コード", "工程名", "作成者"
            ) VALUES (
                @ProcessCode, @ProcessName, @CreatedBy
            )
            ON CONFLICT ("工程コード") DO UPDATE SET
                "工程名" = @ProcessName,
                "更新日時" = CURRENT_TIMESTAMP
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            process.ProcessCode,
            process.ProcessName,
            process.CreatedBy
        });
    }

    public async Task<Process?> FindByProcessCodeAsync(string processCode)
    {
        const string sql = """
            SELECT
                "工程コード" AS ProcessCode,
                "工程名" AS ProcessName,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "工程マスタ"
            WHERE "工程コード" = @ProcessCode
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Process>(sql, new { ProcessCode = processCode });
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync("""DELETE FROM "工程マスタ" """);
    }
}
