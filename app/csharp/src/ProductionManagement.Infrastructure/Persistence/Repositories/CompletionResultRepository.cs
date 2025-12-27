using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 完成実績リポジトリ実装
/// </summary>
public class CompletionResultRepository : ICompletionResultRepository
{
    private readonly string _connectionString;

    public CompletionResultRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(CompletionResult completionResult)
    {
        const string sql = """
            INSERT INTO "完成実績データ" (
                "完成実績番号", "作業指示番号", "品目コード", "完成日",
                "完成数量", "良品数", "不良品数", "備考", "作成者"
            ) VALUES (
                @CompletionResultNumber, @WorkOrderNumber, @ItemCode, @CompletionDate,
                @CompletedQuantity, @GoodQuantity, @DefectQuantity, @Remarks, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        completionResult.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            completionResult.CompletionResultNumber,
            completionResult.WorkOrderNumber,
            completionResult.ItemCode,
            completionResult.CompletionDate,
            completionResult.CompletedQuantity,
            completionResult.GoodQuantity,
            completionResult.DefectQuantity,
            completionResult.Remarks,
            completionResult.CreatedBy
        });
    }

    public async Task<CompletionResult?> FindByCompletionResultNumberAsync(string completionResultNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "完成実績番号" AS CompletionResultNumber,
                "作業指示番号" AS WorkOrderNumber,
                "品目コード" AS ItemCode,
                "完成日" AS CompletionDate,
                "完成数量" AS CompletedQuantity,
                "良品数" AS GoodQuantity,
                "不良品数" AS DefectQuantity,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "完成実績データ"
            WHERE "完成実績番号" = @CompletionResultNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<CompletionResult>(sql, new { CompletionResultNumber = completionResultNumber });
    }

    public async Task<IReadOnlyList<CompletionResult>> FindByWorkOrderNumberAsync(string workOrderNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "完成実績番号" AS CompletionResultNumber,
                "作業指示番号" AS WorkOrderNumber,
                "品目コード" AS ItemCode,
                "完成日" AS CompletionDate,
                "完成数量" AS CompletedQuantity,
                "良品数" AS GoodQuantity,
                "不良品数" AS DefectQuantity,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "完成実績データ"
            WHERE "作業指示番号" = @WorkOrderNumber
            ORDER BY "完成日"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<CompletionResult>(sql, new { WorkOrderNumber = workOrderNumber });
        return result.ToList();
    }

    public async Task<string?> FindLatestCompletionResultNumberAsync(string prefix)
    {
        const string sql = """
            SELECT "完成実績番号" FROM "完成実績データ"
            WHERE "完成実績番号" LIKE @Prefix
            ORDER BY "完成実績番号" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<string>(sql, new { Prefix = prefix });
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync("""DELETE FROM "完成実績データ" """);
    }
}
