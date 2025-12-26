using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 品目リポジトリ実装
/// </summary>
public class ItemRepository : IItemRepository
{
    private readonly string _connectionString;

    static ItemRepository()
    {
        // Dapper TypeHandler の登録
        SqlMapper.AddTypeHandler(new ItemCategoryTypeHandler());
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public ItemRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Item item)
    {
        const string sql = """
            INSERT INTO "品目マスタ" (
                "品目コード", "適用開始日", "適用停止日", "品名", "品目区分",
                "単位コード", "リードタイム", "安全リードタイム", "安全在庫数", "歩留率",
                "最小ロット数", "刻みロット数", "最大ロット数", "有効期間"
            ) VALUES (
                @ItemCode, @EffectiveFrom, @EffectiveTo, @ItemName, @ItemCategory::品目区分,
                @UnitCode, @LeadTime, @SafetyLeadTime, @SafetyStock, @YieldRate,
                @MinLotSize, @LotIncrement, @MaxLotSize, @ShelfLife
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        item.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            item.ItemCode,
            item.EffectiveFrom,
            item.EffectiveTo,
            item.ItemName,
            ItemCategory = item.ItemCategory.GetDisplayName(),
            item.UnitCode,
            item.LeadTime,
            item.SafetyLeadTime,
            item.SafetyStock,
            item.YieldRate,
            item.MinLotSize,
            item.LotIncrement,
            item.MaxLotSize,
            item.ShelfLife
        });
    }

    public async Task<Item?> FindByItemCodeAsync(string itemCode)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "品目コード" as ItemCode,
                "適用開始日" as EffectiveFrom,
                "適用停止日" as EffectiveTo,
                "品名" as ItemName,
                "品目区分"::TEXT as ItemCategoryValue,
                "単位コード" as UnitCode,
                "リードタイム" as LeadTime,
                "安全リードタイム" as SafetyLeadTime,
                "安全在庫数" as SafetyStock,
                "歩留率" as YieldRate,
                "最小ロット数" as MinLotSize,
                "刻みロット数" as LotIncrement,
                "最大ロット数" as MaxLotSize,
                "有効期間" as ShelfLife,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "品目マスタ"
            WHERE "品目コード" = @ItemCode
            ORDER BY "適用開始日" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Item>(sql, new { ItemCode = itemCode });
    }

    public async Task<Item?> FindByItemCodeAndDateAsync(string itemCode, DateOnly baseDate)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "品目コード" as ItemCode,
                "適用開始日" as EffectiveFrom,
                "適用停止日" as EffectiveTo,
                "品名" as ItemName,
                "品目区分"::TEXT as ItemCategoryValue,
                "単位コード" as UnitCode,
                "リードタイム" as LeadTime,
                "安全リードタイム" as SafetyLeadTime,
                "安全在庫数" as SafetyStock,
                "歩留率" as YieldRate,
                "最小ロット数" as MinLotSize,
                "刻みロット数" as LotIncrement,
                "最大ロット数" as MaxLotSize,
                "有効期間" as ShelfLife,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "品目マスタ"
            WHERE "品目コード" = @ItemCode
              AND "適用開始日" <= @BaseDate
              AND ("適用停止日" IS NULL OR "適用停止日" > @BaseDate)
            ORDER BY "適用開始日" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Item>(sql, new { ItemCode = itemCode, BaseDate = baseDate });
    }

    public async Task<IReadOnlyList<Item>> FindAllAsync()
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "品目コード" as ItemCode,
                "適用開始日" as EffectiveFrom,
                "適用停止日" as EffectiveTo,
                "品名" as ItemName,
                "品目区分"::TEXT as ItemCategoryValue,
                "単位コード" as UnitCode,
                "リードタイム" as LeadTime,
                "安全リードタイム" as SafetyLeadTime,
                "安全在庫数" as SafetyStock,
                "歩留率" as YieldRate,
                "最小ロット数" as MinLotSize,
                "刻みロット数" as LotIncrement,
                "最大ロット数" as MaxLotSize,
                "有効期間" as ShelfLife,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "品目マスタ"
            ORDER BY "品目コード", "適用開始日" DESC
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Item>(sql);
        return result.ToList();
    }

    public async Task UpdateAsync(Item item)
    {
        const string sql = """
            UPDATE "品目マスタ" SET
                "品名" = @ItemName,
                "品目区分" = @ItemCategory::品目区分,
                "単位コード" = @UnitCode,
                "リードタイム" = @LeadTime,
                "安全リードタイム" = @SafetyLeadTime,
                "安全在庫数" = @SafetyStock,
                "歩留率" = @YieldRate,
                "最小ロット数" = @MinLotSize,
                "刻みロット数" = @LotIncrement,
                "最大ロット数" = @MaxLotSize,
                "有効期間" = @ShelfLife,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            item.Id,
            item.ItemName,
            ItemCategory = item.ItemCategory.GetDisplayName(),
            item.UnitCode,
            item.LeadTime,
            item.SafetyLeadTime,
            item.SafetyStock,
            item.YieldRate,
            item.MinLotSize,
            item.LotIncrement,
            item.MaxLotSize,
            item.ShelfLife
        });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """TRUNCATE TABLE "品目マスタ" CASCADE""";

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
