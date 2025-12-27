using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Quality;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// ロットリポジトリ
/// </summary>
public class LotRepository : ILotRepository
{
    private readonly string _connectionString;

    static LotRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
        SqlMapper.AddTypeHandler(new LotTypeTypeHandler());
    }

    public LotRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    private NpgsqlConnection CreateConnection() => new(_connectionString);

    public async Task<LotMaster?> FindByLotNumberAsync(string lotNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "ロット番号" AS LotNumber,
                "品目コード" AS ItemCode,
                "ロット種別"::TEXT AS LotTypeValue,
                "製造日" AS ManufactureDate,
                "有効期限" AS ExpirationDate,
                "数量" AS Quantity,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "ロットマスタ"
            WHERE "ロット番号" = @LotNumber
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<LotMaster>(sql, new { LotNumber = lotNumber });
    }

    public async Task<IReadOnlyList<LotMaster>> FindByItemCodeAsync(string itemCode)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "ロット番号" AS LotNumber,
                "品目コード" AS ItemCode,
                "ロット種別"::TEXT AS LotTypeValue,
                "製造日" AS ManufactureDate,
                "有効期限" AS ExpirationDate,
                "数量" AS Quantity,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "ロットマスタ"
            WHERE "品目コード" = @ItemCode
            ORDER BY "ロット番号"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<LotMaster>(sql, new { ItemCode = itemCode });
        return result.ToList();
    }

    public async Task<IReadOnlyList<LotComposition>> FindChildLotsAsync(string parentLotNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "親ロット番号" AS ParentLotNumber,
                "子ロット番号" AS ChildLotNumber,
                "使用数量" AS UsedQuantity
            FROM "ロット構成"
            WHERE "親ロット番号" = @ParentLotNumber
            ORDER BY "子ロット番号"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<LotComposition>(sql, new { ParentLotNumber = parentLotNumber });
        return result.ToList();
    }

    public async Task<IReadOnlyList<LotComposition>> FindParentLotsAsync(string childLotNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "親ロット番号" AS ParentLotNumber,
                "子ロット番号" AS ChildLotNumber,
                "使用数量" AS UsedQuantity
            FROM "ロット構成"
            WHERE "子ロット番号" = @ChildLotNumber
            ORDER BY "親ロット番号"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<LotComposition>(sql, new { ChildLotNumber = childLotNumber });
        return result.ToList();
    }

    public async Task<long> SaveAsync(LotMaster lot)
    {
        const string sql = """
            INSERT INTO "ロットマスタ" (
                "ロット番号", "品目コード", "ロット種別",
                "製造日", "有効期限", "数量"
            ) VALUES (
                @LotNumber, @ItemCode, @LotType::ロット種別,
                @ManufactureDate, @ExpirationDate, @Quantity
            )
            RETURNING "ID"
            """;

        await using var connection = CreateConnection();
        return await connection.ExecuteScalarAsync<long>(sql, new
        {
            lot.LotNumber,
            lot.ItemCode,
            LotType = lot.LotType.ToDisplayName(),
            lot.ManufactureDate,
            lot.ExpirationDate,
            lot.Quantity
        });
    }

    public async Task SaveCompositionAsync(LotComposition composition)
    {
        const string sql = """
            INSERT INTO "ロット構成" (
                "親ロット番号", "子ロット番号", "使用数量"
            ) VALUES (
                @ParentLotNumber, @ChildLotNumber, @UsedQuantity
            )
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, composition);
    }

    public async Task DeleteAllCompositionsAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "ロット構成" """);
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "ロットマスタ" """);
    }
}
