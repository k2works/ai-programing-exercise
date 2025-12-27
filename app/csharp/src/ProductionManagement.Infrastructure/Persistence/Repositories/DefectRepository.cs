using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 欠点マスタリポジトリ実装
/// </summary>
public class DefectRepository : IDefectRepository
{
    private readonly string _connectionString;

    public DefectRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Defect defect)
    {
        const string sql = """
            INSERT INTO "欠点マスタ" (
                "欠点コード", "欠点名", "欠点区分", "表示順", "有効フラグ", "作成者"
            ) VALUES (
                @DefectCode, @DefectName, @DefectCategory, @DisplayOrder, @IsActive, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        defect.Id = await connection.ExecuteScalarAsync<int>(sql, defect);
    }

    public async Task<Defect?> FindByIdAsync(int id)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "欠点コード" AS DefectCode,
                "欠点名" AS DefectName,
                "欠点区分" AS DefectCategory,
                "表示順" AS DisplayOrder,
                "有効フラグ" AS IsActive,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "欠点マスタ"
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Defect>(sql, new { Id = id });
    }

    public async Task<Defect?> FindByDefectCodeAsync(string defectCode)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "欠点コード" AS DefectCode,
                "欠点名" AS DefectName,
                "欠点区分" AS DefectCategory,
                "表示順" AS DisplayOrder,
                "有効フラグ" AS IsActive,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "欠点マスタ"
            WHERE "欠点コード" = @DefectCode
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Defect>(
            sql, new { DefectCode = defectCode });
    }

    public async Task<IReadOnlyList<Defect>> FindAllActiveAsync()
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "欠点コード" AS DefectCode,
                "欠点名" AS DefectName,
                "欠点区分" AS DefectCategory,
                "表示順" AS DisplayOrder,
                "有効フラグ" AS IsActive,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "欠点マスタ"
            WHERE "有効フラグ" = true
            ORDER BY "表示順", "欠点コード"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var results = await connection.QueryAsync<Defect>(sql);
        return results.ToList();
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "欠点マスタ" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
