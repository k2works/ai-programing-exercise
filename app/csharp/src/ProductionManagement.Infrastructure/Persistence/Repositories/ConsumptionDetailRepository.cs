using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Subcontract;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 消費明細リポジトリ実装
/// </summary>
public class ConsumptionDetailRepository : IConsumptionDetailRepository
{
    private readonly string _connectionString;

    static ConsumptionDetailRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public ConsumptionDetailRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(ConsumptionDetail detail)
    {
        const string sql = """
            INSERT INTO "消費明細データ" (
                "消費番号", "消費行番号", "品目コード", "消費数量", "備考"
            ) VALUES (
                @ConsumptionNumber, @LineNumber, @ItemCode, @Quantity, @Remarks
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        detail.Id = await connection.ExecuteScalarAsync<int>(sql, detail);
    }

    public async Task<IReadOnlyList<ConsumptionDetail>> FindByConsumptionNumberAsync(string consumptionNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "消費番号" AS ConsumptionNumber,
                "消費行番号" AS LineNumber,
                "品目コード" AS ItemCode,
                "消費数量" AS Quantity,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "消費明細データ"
            WHERE "消費番号" = @ConsumptionNumber
            ORDER BY "消費行番号"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var results = await connection.QueryAsync<ConsumptionDetail>(
            sql, new { ConsumptionNumber = consumptionNumber });
        return results.ToList();
    }

    public async Task<decimal> SumByPurchaseOrderAndItemAsync(
        string purchaseOrderNumber, int lineNumber, string itemCode)
    {
        const string sql = """
            SELECT COALESCE(SUM(cd."消費数量"), 0)
            FROM "消費明細データ" cd
            JOIN "消費データ" c ON cd."消費番号" = c."消費番号"
            JOIN "入荷受入データ" r ON c."入荷番号" = r."入荷番号"
            WHERE r."発注番号" = @PurchaseOrderNumber
              AND r."発注行番号" = @LineNumber
              AND cd."品目コード" = @ItemCode
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.ExecuteScalarAsync<decimal>(sql, new
        {
            PurchaseOrderNumber = purchaseOrderNumber,
            LineNumber = lineNumber,
            ItemCode = itemCode
        });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "消費明細データ" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
