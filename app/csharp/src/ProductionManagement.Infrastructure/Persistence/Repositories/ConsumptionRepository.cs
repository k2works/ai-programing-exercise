using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Subcontract;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 消費リポジトリ実装
/// </summary>
public class ConsumptionRepository : IConsumptionRepository
{
    private readonly string _connectionString;

    static ConsumptionRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public ConsumptionRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Consumption consumption)
    {
        const string sql = """
            INSERT INTO "消費データ" (
                "消費番号", "入荷番号", "消費日", "取引先コード", "備考", "作成者"
            ) VALUES (
                @ConsumptionNumber, @ReceivingNumber, @ConsumptionDate, @SupplierCode, @Remarks, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        consumption.Id = await connection.ExecuteScalarAsync<int>(sql, consumption);
    }

    public async Task<Consumption?> FindByIdAsync(int id)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "消費番号" AS ConsumptionNumber,
                "入荷番号" AS ReceivingNumber,
                "消費日" AS ConsumptionDate,
                "取引先コード" AS SupplierCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "消費データ"
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Consumption>(sql, new { Id = id });
    }

    public async Task<Consumption?> FindByConsumptionNumberAsync(string consumptionNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "消費番号" AS ConsumptionNumber,
                "入荷番号" AS ReceivingNumber,
                "消費日" AS ConsumptionDate,
                "取引先コード" AS SupplierCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "消費データ"
            WHERE "消費番号" = @ConsumptionNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Consumption>(
            sql, new { ConsumptionNumber = consumptionNumber });
    }

    public async Task<Consumption?> FindByReceivingNumberAsync(string receivingNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "消費番号" AS ConsumptionNumber,
                "入荷番号" AS ReceivingNumber,
                "消費日" AS ConsumptionDate,
                "取引先コード" AS SupplierCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "消費データ"
            WHERE "入荷番号" = @ReceivingNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Consumption>(
            sql, new { ReceivingNumber = receivingNumber });
    }

    public async Task<string?> FindLatestConsumptionNumberAsync(string prefix)
    {
        const string sql = """
            SELECT "消費番号"
            FROM "消費データ"
            WHERE "消費番号" LIKE @Prefix
            ORDER BY "消費番号" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<string>(sql, new { Prefix = prefix });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "消費データ" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
