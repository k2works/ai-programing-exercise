using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Inventory;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 棚卸リポジトリ
/// </summary>
public class StocktakingRepository : IStocktakingRepository
{
    private readonly string _connectionString;

    static StocktakingRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
        SqlMapper.AddTypeHandler(new StocktakingStatusTypeHandler());
    }

    public StocktakingRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    private NpgsqlConnection CreateConnection() => new(_connectionString);

    public async Task<Stocktaking?> FindByStocktakingNumberAsync(string stocktakingNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "棚卸番号" AS StocktakingNumber,
                "場所コード" AS LocationCode,
                "棚卸日" AS StocktakingDate,
                "ステータス"::TEXT AS StatusValue,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "棚卸データ"
            WHERE "棚卸番号" = @StocktakingNumber
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<Stocktaking>(sql, new { StocktakingNumber = stocktakingNumber });
    }

    public async Task<IReadOnlyList<StocktakingDetail>> FindDetailsByStocktakingNumberAsync(string stocktakingNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "棚卸番号" AS StocktakingNumber,
                "棚卸行番号" AS LineNumber,
                "品目コード" AS ItemCode,
                "帳簿数量" AS BookQuantity,
                "実棚数量" AS ActualQuantity,
                "差異数量" AS DifferenceQuantity,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "棚卸明細データ"
            WHERE "棚卸番号" = @StocktakingNumber
            ORDER BY "棚卸行番号"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<StocktakingDetail>(sql, new { StocktakingNumber = stocktakingNumber });
        return result.ToList();
    }

    public async Task<string?> FindLatestStocktakingNumberAsync(string pattern)
    {
        const string sql = """
            SELECT "棚卸番号"
            FROM "棚卸データ"
            WHERE "棚卸番号" LIKE @Pattern
            ORDER BY "棚卸番号" DESC
            LIMIT 1
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<string>(sql, new { Pattern = pattern });
    }

    public async Task<long> SaveAsync(Stocktaking stocktaking)
    {
        const string sql = """
            INSERT INTO "棚卸データ" (
                "棚卸番号", "場所コード", "棚卸日", "ステータス"
            ) VALUES (
                @StocktakingNumber, @LocationCode, @StocktakingDate, @Status::棚卸ステータス
            )
            RETURNING "ID"
            """;

        await using var connection = CreateConnection();
        return await connection.ExecuteScalarAsync<long>(sql, new
        {
            stocktaking.StocktakingNumber,
            stocktaking.LocationCode,
            stocktaking.StocktakingDate,
            Status = stocktaking.Status.ToDisplayName()
        });
    }

    public async Task SaveDetailAsync(StocktakingDetail detail)
    {
        const string sql = """
            INSERT INTO "棚卸明細データ" (
                "棚卸番号", "棚卸行番号", "品目コード", "帳簿数量", "実棚数量", "差異数量"
            ) VALUES (
                @StocktakingNumber, @LineNumber, @ItemCode, @BookQuantity, @ActualQuantity, @DifferenceQuantity
            )
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, detail);
    }

    public async Task UpdateDetailAsync(long id, decimal actualQuantity, decimal differenceQuantity)
    {
        const string sql = """
            UPDATE "棚卸明細データ"
            SET "実棚数量" = @ActualQuantity,
                "差異数量" = @DifferenceQuantity,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "ID" = @Id
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, new { Id = id, ActualQuantity = actualQuantity, DifferenceQuantity = differenceQuantity });
    }

    public async Task UpdateStatusAsync(string stocktakingNumber, StocktakingStatus status)
    {
        const string sql = """
            UPDATE "棚卸データ"
            SET "ステータス" = @Status::棚卸ステータス,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "棚卸番号" = @StocktakingNumber
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, new { StocktakingNumber = stocktakingNumber, Status = status.ToDisplayName() });
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "棚卸データ" """);
    }

    public async Task DeleteAllDetailsAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "棚卸明細データ" """);
    }
}
