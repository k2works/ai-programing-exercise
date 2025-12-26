using Dapper;
using Npgsql;
using ProductionManagement.Domain.Models;

namespace ProductionManagement.Infrastructure.Repositories;

/// <summary>
/// 品目マスタリポジトリ
/// </summary>
public class ItemRepository
{
    private readonly string _connectionString;

    public ItemRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task<int> InsertAsync(Item item)
    {
        const string sql = """
            INSERT INTO 品目マスタ (
                品目コード, 適用開始日, 適用停止日, 品名, 品目区分,
                単位コード, リードタイム, 安全リードタイム, 安全在庫数, 歩留率,
                最小ロット数, 刻みロット数, 最大ロット数, 有効期間
            ) VALUES (
                @ItemCode, @EffectiveFrom, @EffectiveTo, @ItemName, @ItemCategory::品目区分,
                @UnitCode, @LeadTime, @SafetyLeadTime, @SafetyStock, @YieldRate,
                @MinLotSize, @LotIncrement, @MaxLotSize, @ShelfLife
            )
            RETURNING id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.ExecuteScalarAsync<int>(sql, item);
    }

    public async Task<Item?> FindByItemCodeAsync(string itemCode)
    {
        const string sql = "SELECT * FROM 品目マスタ WHERE 品目コード = @ItemCode";

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Item>(sql, new { ItemCode = itemCode });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = "TRUNCATE TABLE 品目マスタ CASCADE";

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
