using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 完成検査結果リポジトリ実装
/// </summary>
public class CompletionInspectionResultRepository : ICompletionInspectionResultRepository
{
    private readonly string _connectionString;

    public CompletionInspectionResultRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(CompletionInspectionResult inspectionResult)
    {
        const string sql = """
            INSERT INTO "完成検査結果データ" (
                "完成実績番号", "欠点コード", "数量"
            ) VALUES (
                @CompletionResultNumber, @DefectCode, @Quantity
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        inspectionResult.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            inspectionResult.CompletionResultNumber,
            inspectionResult.DefectCode,
            inspectionResult.Quantity
        });
    }

    public async Task<IReadOnlyList<CompletionInspectionResult>> FindByCompletionResultNumberAsync(string completionResultNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "完成実績番号" AS CompletionResultNumber,
                "欠点コード" AS DefectCode,
                "数量" AS Quantity,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "完成検査結果データ"
            WHERE "完成実績番号" = @CompletionResultNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<CompletionInspectionResult>(sql, new { CompletionResultNumber = completionResultNumber });
        return result.ToList();
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync("""DELETE FROM "完成検査結果データ" """);
    }
}
