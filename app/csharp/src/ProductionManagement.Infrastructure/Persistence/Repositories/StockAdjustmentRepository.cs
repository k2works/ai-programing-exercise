using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Inventory;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 在庫調整リポジトリ
/// </summary>
public class StockAdjustmentRepository : IStockAdjustmentRepository
{
    private readonly string _connectionString;

    static StockAdjustmentRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public StockAdjustmentRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    private NpgsqlConnection CreateConnection() => new(_connectionString);

    public async Task<IReadOnlyList<StockAdjustment>> FindByStocktakingNumberAsync(string stocktakingNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "在庫調整番号" AS AdjustmentNumber,
                "棚卸番号" AS StocktakingNumber,
                "品目コード" AS ItemCode,
                "場所コード" AS LocationCode,
                "調整日" AS AdjustmentDate,
                "調整担当者コード" AS AdjusterCode,
                "調整数" AS AdjustmentQuantity,
                "理由コード" AS ReasonCode,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "在庫調整データ"
            WHERE "棚卸番号" = @StocktakingNumber
            ORDER BY "調整日", "在庫調整番号"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<StockAdjustment>(sql, new { StocktakingNumber = stocktakingNumber });
        return result.ToList();
    }

    public async Task<long> SaveAsync(StockAdjustment adjustment)
    {
        const string sql = """
            INSERT INTO "在庫調整データ" (
                "在庫調整番号", "棚卸番号", "品目コード", "場所コード",
                "調整日", "調整担当者コード", "調整数", "理由コード"
            ) VALUES (
                @AdjustmentNumber, @StocktakingNumber, @ItemCode, @LocationCode,
                @AdjustmentDate, @AdjusterCode, @AdjustmentQuantity, @ReasonCode
            )
            RETURNING "ID"
            """;

        await using var connection = CreateConnection();
        return await connection.ExecuteScalarAsync<long>(sql, adjustment);
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "在庫調整データ" """);
    }
}
