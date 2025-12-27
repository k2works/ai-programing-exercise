using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 検収リポジトリ実装
/// </summary>
public class AcceptanceRepository : IAcceptanceRepository
{
    private readonly string _connectionString;

    static AcceptanceRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public AcceptanceRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Acceptance acceptance)
    {
        const string sql = """
            INSERT INTO "検収データ" (
                "検収番号", "検査番号", "検収日", "検収数量",
                "検収金額", "消費税金額", "入庫場所コード", "備考", "作成者"
            ) VALUES (
                @AcceptanceNumber, @InspectionNumber, @AcceptanceDate, @AcceptanceQuantity,
                @AcceptanceAmount, @TaxAmount, @StorageLocationCode, @Remarks, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        acceptance.Id = await connection.ExecuteScalarAsync<int>(sql, acceptance);
    }

    public async Task<Acceptance?> FindByIdAsync(int id)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "検収番号" AS AcceptanceNumber,
                "検査番号" AS InspectionNumber,
                "検収日" AS AcceptanceDate,
                "検収数量" AS AcceptanceQuantity,
                "検収金額" AS AcceptanceAmount,
                "消費税金額" AS TaxAmount,
                "入庫場所コード" AS StorageLocationCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "検収データ"
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Acceptance>(sql, new { Id = id });
    }

    public async Task<Acceptance?> FindByAcceptanceNumberAsync(string acceptanceNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "検収番号" AS AcceptanceNumber,
                "検査番号" AS InspectionNumber,
                "検収日" AS AcceptanceDate,
                "検収数量" AS AcceptanceQuantity,
                "検収金額" AS AcceptanceAmount,
                "消費税金額" AS TaxAmount,
                "入庫場所コード" AS StorageLocationCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "検収データ"
            WHERE "検収番号" = @AcceptanceNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Acceptance>(
            sql, new { AcceptanceNumber = acceptanceNumber });
    }

    public async Task<IReadOnlyList<Acceptance>> FindByInspectionNumberAsync(string inspectionNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "検収番号" AS AcceptanceNumber,
                "検査番号" AS InspectionNumber,
                "検収日" AS AcceptanceDate,
                "検収数量" AS AcceptanceQuantity,
                "検収金額" AS AcceptanceAmount,
                "消費税金額" AS TaxAmount,
                "入庫場所コード" AS StorageLocationCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "検収データ"
            WHERE "検査番号" = @InspectionNumber
            ORDER BY "検収番号"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var results = await connection.QueryAsync<Acceptance>(
            sql, new { InspectionNumber = inspectionNumber });
        return results.ToList();
    }

    public async Task<string?> FindLatestAcceptanceNumberAsync(string prefix)
    {
        const string sql = """
            SELECT "検収番号"
            FROM "検収データ"
            WHERE "検収番号" LIKE @Prefix
            ORDER BY "検収番号" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<string>(sql, new { Prefix = prefix });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "検収データ" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
