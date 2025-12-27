using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 受入検査リポジトリ実装
/// </summary>
public class InspectionRepository : IInspectionRepository
{
    private readonly string _connectionString;

    static InspectionRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
        SqlMapper.AddTypeHandler(new InspectionResultTypeHandler());
    }

    public InspectionRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Inspection inspection)
    {
        const string sql = """
            INSERT INTO "受入検査データ" (
                "検査番号", "入荷番号", "検査日", "検査数量",
                "合格数量", "不合格数量", "検査結果", "欠点コード",
                "検査担当者コード", "備考", "作成者"
            ) VALUES (
                @InspectionNumber, @ReceivingNumber, @InspectionDate, @InspectionQuantity,
                @PassedQuantity, @FailedQuantity, @InspectionResult::検査結果区分, @DefectCode,
                @InspectorCode, @Remarks, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        inspection.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            inspection.InspectionNumber,
            inspection.ReceivingNumber,
            inspection.InspectionDate,
            inspection.InspectionQuantity,
            inspection.PassedQuantity,
            inspection.FailedQuantity,
            InspectionResult = inspection.InspectionResult.GetDisplayName(),
            inspection.DefectCode,
            inspection.InspectorCode,
            inspection.Remarks,
            inspection.CreatedBy
        });
    }

    public async Task<Inspection?> FindByIdAsync(int id)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "検査番号" AS InspectionNumber,
                "入荷番号" AS ReceivingNumber,
                "検査日" AS InspectionDate,
                "検査数量" AS InspectionQuantity,
                "合格数量" AS PassedQuantity,
                "不合格数量" AS FailedQuantity,
                "検査結果"::TEXT AS InspectionResultValue,
                "欠点コード" AS DefectCode,
                "検査担当者コード" AS InspectorCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "受入検査データ"
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Inspection>(sql, new { Id = id });
    }

    public async Task<Inspection?> FindByInspectionNumberAsync(string inspectionNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "検査番号" AS InspectionNumber,
                "入荷番号" AS ReceivingNumber,
                "検査日" AS InspectionDate,
                "検査数量" AS InspectionQuantity,
                "合格数量" AS PassedQuantity,
                "不合格数量" AS FailedQuantity,
                "検査結果"::TEXT AS InspectionResultValue,
                "欠点コード" AS DefectCode,
                "検査担当者コード" AS InspectorCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "受入検査データ"
            WHERE "検査番号" = @InspectionNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Inspection>(
            sql, new { InspectionNumber = inspectionNumber });
    }

    public async Task<IReadOnlyList<Inspection>> FindByReceivingNumberAsync(string receivingNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "検査番号" AS InspectionNumber,
                "入荷番号" AS ReceivingNumber,
                "検査日" AS InspectionDate,
                "検査数量" AS InspectionQuantity,
                "合格数量" AS PassedQuantity,
                "不合格数量" AS FailedQuantity,
                "検査結果"::TEXT AS InspectionResultValue,
                "欠点コード" AS DefectCode,
                "検査担当者コード" AS InspectorCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "受入検査データ"
            WHERE "入荷番号" = @ReceivingNumber
            ORDER BY "検査番号"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var results = await connection.QueryAsync<Inspection>(
            sql, new { ReceivingNumber = receivingNumber });
        return results.ToList();
    }

    public async Task<string?> FindLatestInspectionNumberAsync(string prefix)
    {
        const string sql = """
            SELECT "検査番号"
            FROM "受入検査データ"
            WHERE "検査番号" LIKE @Prefix
            ORDER BY "検査番号" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<string>(sql, new { Prefix = prefix });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "受入検査データ" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
