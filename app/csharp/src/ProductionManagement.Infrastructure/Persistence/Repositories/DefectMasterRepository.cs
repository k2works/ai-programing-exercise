using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Quality;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 欠点マスタリポジトリ
/// </summary>
public class DefectMasterRepository : IDefectMasterRepository
{
    private readonly string _connectionString;

    public DefectMasterRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    private NpgsqlConnection CreateConnection() => new(_connectionString);

    public async Task<Defect?> FindByDefectCodeAsync(string defectCode)
    {
        const string sql = """
            SELECT
                "欠点コード" AS DefectCode,
                "欠点名" AS DefectName,
                "欠点区分" AS DefectCategory,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "欠点マスタ"
            WHERE "欠点コード" = @DefectCode
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<Defect>(sql, new { DefectCode = defectCode });
    }

    public async Task<IReadOnlyList<Defect>> FindAllAsync()
    {
        const string sql = """
            SELECT
                "欠点コード" AS DefectCode,
                "欠点名" AS DefectName,
                "欠点区分" AS DefectCategory,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "欠点マスタ"
            ORDER BY "欠点コード"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<Defect>(sql);
        return result.ToList();
    }

    public async Task SaveAsync(Defect defect)
    {
        const string sql = """
            INSERT INTO "欠点マスタ" (
                "欠点コード", "欠点名", "欠点区分"
            ) VALUES (
                @DefectCode, @DefectName, @DefectCategory
            )
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, defect);
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "欠点マスタ" """);
    }
}
