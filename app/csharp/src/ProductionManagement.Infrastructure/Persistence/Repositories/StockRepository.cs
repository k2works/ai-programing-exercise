using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 在庫情報リポジトリ
/// </summary>
public class StockRepository : IStockRepository
{
    private readonly string _connectionString;

    public StockRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    private NpgsqlConnection CreateConnection() => new(_connectionString);

    public async Task<Stock?> FindByLocationAndItemAsync(string locationCode, string itemCode)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "場所コード" AS LocationCode,
                "品目コード" AS ItemCode,
                "在庫数量" AS StockQuantity,
                "合格数" AS PassedQuantity,
                "不良数" AS DefectiveQuantity,
                "未検査数" AS UninspectedQuantity,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "在庫情報"
            WHERE "場所コード" = @LocationCode AND "品目コード" = @ItemCode
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<Stock>(sql, new { LocationCode = locationCode, ItemCode = itemCode });
    }

    public async Task<IReadOnlyList<Stock>> FindByLocationAsync(string locationCode)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "場所コード" AS LocationCode,
                "品目コード" AS ItemCode,
                "在庫数量" AS StockQuantity,
                "合格数" AS PassedQuantity,
                "不良数" AS DefectiveQuantity,
                "未検査数" AS UninspectedQuantity,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "在庫情報"
            WHERE "場所コード" = @LocationCode
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<Stock>(sql, new { LocationCode = locationCode });
        return result.ToList();
    }

    public async Task<IReadOnlyList<Stock>> FindAllAsync()
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "場所コード" AS LocationCode,
                "品目コード" AS ItemCode,
                "在庫数量" AS StockQuantity,
                "合格数" AS PassedQuantity,
                "不良数" AS DefectiveQuantity,
                "未検査数" AS UninspectedQuantity,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "在庫情報"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<Stock>(sql);
        return result.ToList();
    }

    public async Task<long> SaveAsync(Stock stock)
    {
        const string sql = """
            INSERT INTO "在庫情報" (
                "場所コード", "品目コード", "在庫数量",
                "合格数", "不良数", "未検査数"
            ) VALUES (
                @LocationCode, @ItemCode, @StockQuantity,
                @PassedQuantity, @DefectiveQuantity, @UninspectedQuantity
            )
            RETURNING "ID"
            """;

        await using var connection = CreateConnection();
        return await connection.ExecuteScalarAsync<long>(sql, stock);
    }

    public async Task IncreaseByStatusAsync(string locationCode, string itemCode, decimal quantity, StockStatus status)
    {
        var statusColumn = GetStatusColumn(status);
        var sql = $"""
            UPDATE "在庫情報"
            SET "在庫数量" = "在庫数量" + @Quantity,
                "{statusColumn}" = "{statusColumn}" + @Quantity,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "場所コード" = @LocationCode AND "品目コード" = @ItemCode
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, new { LocationCode = locationCode, ItemCode = itemCode, Quantity = quantity });
    }

    public async Task DecreaseByStatusAsync(string locationCode, string itemCode, decimal quantity, StockStatus status)
    {
        var statusColumn = GetStatusColumn(status);
        var sql = $"""
            UPDATE "在庫情報"
            SET "在庫数量" = "在庫数量" - @Quantity,
                "{statusColumn}" = "{statusColumn}" - @Quantity,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "場所コード" = @LocationCode AND "品目コード" = @ItemCode
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, new { LocationCode = locationCode, ItemCode = itemCode, Quantity = quantity });
    }

    public async Task ChangeStatusAsync(string locationCode, string itemCode, decimal quantity, StockStatus fromStatus, StockStatus toStatus)
    {
        var fromColumn = GetStatusColumn(fromStatus);
        var toColumn = GetStatusColumn(toStatus);
        var sql = $"""
            UPDATE "在庫情報"
            SET "{fromColumn}" = "{fromColumn}" - @Quantity,
                "{toColumn}" = "{toColumn}" + @Quantity,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "場所コード" = @LocationCode AND "品目コード" = @ItemCode
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, new { LocationCode = locationCode, ItemCode = itemCode, Quantity = quantity });
    }

    public async Task UpdateAsync(Stock stock)
    {
        const string sql = """
            UPDATE "在庫情報"
            SET "在庫数量" = @StockQuantity,
                "合格数" = @PassedQuantity,
                "不良数" = @DefectiveQuantity,
                "未検査数" = @UninspectedQuantity,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "場所コード" = @LocationCode AND "品目コード" = @ItemCode
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, stock);
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "在庫情報" """);
    }

    private static string GetStatusColumn(StockStatus status) => status switch
    {
        StockStatus.Passed => "合格数",
        StockStatus.Defective => "不良数",
        StockStatus.Uninspected => "未検査数",
        _ => throw new ArgumentOutOfRangeException(nameof(status))
    };
}
