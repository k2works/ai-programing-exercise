using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Quality;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 出荷検査リポジトリ
/// </summary>
public class ShipmentInspectionRepository : IShipmentInspectionRepository
{
    private readonly string _connectionString;

    static ShipmentInspectionRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
        SqlMapper.AddTypeHandler(new InspectionJudgmentTypeHandler());
    }

    public ShipmentInspectionRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    private NpgsqlConnection CreateConnection() => new(_connectionString);

    public async Task<ShipmentInspection?> FindByInspectionNumberAsync(string inspectionNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "出荷検査番号" AS InspectionNumber,
                "出荷番号" AS ShipmentNumber,
                "品目コード" AS ItemCode,
                "検査日" AS InspectionDate,
                "検査担当者コード" AS InspectorCode,
                "検査数量" AS InspectionQuantity,
                "合格数" AS PassedQuantity,
                "不合格数" AS FailedQuantity,
                "判定"::TEXT AS JudgmentValue,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "出荷検査データ"
            WHERE "出荷検査番号" = @InspectionNumber
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<ShipmentInspection>(sql, new { InspectionNumber = inspectionNumber });
    }

    public async Task<IReadOnlyList<ShipmentInspection>> FindByShipmentNumberAsync(string shipmentNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "出荷検査番号" AS InspectionNumber,
                "出荷番号" AS ShipmentNumber,
                "品目コード" AS ItemCode,
                "検査日" AS InspectionDate,
                "検査担当者コード" AS InspectorCode,
                "検査数量" AS InspectionQuantity,
                "合格数" AS PassedQuantity,
                "不合格数" AS FailedQuantity,
                "判定"::TEXT AS JudgmentValue,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "出荷検査データ"
            WHERE "出荷番号" = @ShipmentNumber
            ORDER BY "検査日", "出荷検査番号"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<ShipmentInspection>(sql, new { ShipmentNumber = shipmentNumber });
        return result.ToList();
    }

    public async Task<IReadOnlyList<ShipmentInspectionResult>> FindResultsByInspectionNumberAsync(string inspectionNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "出荷検査番号" AS InspectionNumber,
                "欠点コード" AS DefectCode,
                "数量" AS Quantity
            FROM "出荷検査結果データ"
            WHERE "出荷検査番号" = @InspectionNumber
            ORDER BY "欠点コード"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<ShipmentInspectionResult>(sql, new { InspectionNumber = inspectionNumber });
        return result.ToList();
    }

    public async Task<string?> FindLatestInspectionNumberAsync(string pattern)
    {
        const string sql = """
            SELECT "出荷検査番号"
            FROM "出荷検査データ"
            WHERE "出荷検査番号" LIKE @Pattern
            ORDER BY "出荷検査番号" DESC
            LIMIT 1
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<string>(sql, new { Pattern = pattern });
    }

    public async Task<long> SaveAsync(ShipmentInspection inspection)
    {
        const string sql = """
            INSERT INTO "出荷検査データ" (
                "出荷検査番号", "出荷番号", "品目コード", "検査日",
                "検査担当者コード", "検査数量", "合格数", "不合格数", "判定"
            ) VALUES (
                @InspectionNumber, @ShipmentNumber, @ItemCode, @InspectionDate,
                @InspectorCode, @InspectionQuantity, @PassedQuantity, @FailedQuantity,
                @Judgment::検査判定
            )
            RETURNING "ID"
            """;

        await using var connection = CreateConnection();
        return await connection.ExecuteScalarAsync<long>(sql, new
        {
            inspection.InspectionNumber,
            inspection.ShipmentNumber,
            inspection.ItemCode,
            inspection.InspectionDate,
            inspection.InspectorCode,
            inspection.InspectionQuantity,
            inspection.PassedQuantity,
            inspection.FailedQuantity,
            Judgment = inspection.Judgment.ToDisplayName()
        });
    }

    public async Task SaveResultAsync(ShipmentInspectionResult result)
    {
        const string sql = """
            INSERT INTO "出荷検査結果データ" (
                "出荷検査番号", "欠点コード", "数量"
            ) VALUES (
                @InspectionNumber, @DefectCode, @Quantity
            )
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, result);
    }

    public async Task DeleteAllResultsAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "出荷検査結果データ" """);
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "出荷検査データ" """);
    }
}
