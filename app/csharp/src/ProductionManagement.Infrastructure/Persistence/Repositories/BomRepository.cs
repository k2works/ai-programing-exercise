using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Bom;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// BOMリポジトリ実装
/// </summary>
public class BomRepository : IBomRepository
{
    private readonly string _connectionString;

    static BomRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public BomRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Bom bom)
    {
        const string sql = """
            INSERT INTO "部品構成表" (
                "親品目コード", "子品目コード", "適用開始日", "適用停止日",
                "基準数量", "必要数量", "不良率", "工順"
            ) VALUES (
                @ParentItemCode, @ChildItemCode, @EffectiveFrom, @EffectiveTo,
                @BaseQuantity, @RequiredQuantity, @DefectRate, @Sequence
            )
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, bom);
    }

    public async Task<IReadOnlyList<Bom>> FindByParentItemCodeAsync(string parentItemCode)
    {
        const string sql = """
            SELECT
                "親品目コード" as ParentItemCode,
                "子品目コード" as ChildItemCode,
                "適用開始日" as EffectiveFrom,
                "適用停止日" as EffectiveTo,
                "基準数量" as BaseQuantity,
                "必要数量" as RequiredQuantity,
                "不良率" as DefectRate,
                "工順" as Sequence,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "部品構成表"
            WHERE "親品目コード" = @ParentItemCode
              AND ("適用停止日" IS NULL OR "適用停止日" > CURRENT_DATE)
            ORDER BY "工順", "子品目コード"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Bom>(sql, new { ParentItemCode = parentItemCode });
        return result.ToList();
    }

    public async Task<IReadOnlyList<Bom>> FindByParentItemCodeAndDateAsync(string parentItemCode, DateOnly baseDate)
    {
        const string sql = """
            SELECT
                "親品目コード" as ParentItemCode,
                "子品目コード" as ChildItemCode,
                "適用開始日" as EffectiveFrom,
                "適用停止日" as EffectiveTo,
                "基準数量" as BaseQuantity,
                "必要数量" as RequiredQuantity,
                "不良率" as DefectRate,
                "工順" as Sequence,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "部品構成表"
            WHERE "親品目コード" = @ParentItemCode
              AND "適用開始日" <= @BaseDate
              AND ("適用停止日" IS NULL OR "適用停止日" > @BaseDate)
            ORDER BY "工順", "子品目コード"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Bom>(sql, new { ParentItemCode = parentItemCode, BaseDate = baseDate });
        return result.ToList();
    }

    public async Task<IReadOnlyList<Bom>> FindByChildItemCodeAsync(string childItemCode)
    {
        const string sql = """
            SELECT
                "親品目コード" as ParentItemCode,
                "子品目コード" as ChildItemCode,
                "適用開始日" as EffectiveFrom,
                "適用停止日" as EffectiveTo,
                "基準数量" as BaseQuantity,
                "必要数量" as RequiredQuantity,
                "不良率" as DefectRate,
                "工順" as Sequence,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "部品構成表"
            WHERE "子品目コード" = @ChildItemCode
              AND ("適用停止日" IS NULL OR "適用停止日" > CURRENT_DATE)
            ORDER BY "親品目コード"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Bom>(sql, new { ChildItemCode = childItemCode });
        return result.ToList();
    }

    public async Task<IReadOnlyList<BomExplosion>> ExplodeAsync(string itemCode, decimal quantity)
    {
        const string sql = """
            WITH RECURSIVE bom_explosion AS (
                -- 基底: 直接の子品目
                SELECT
                    "親品目コード",
                    "子品目コード",
                    "適用開始日",
                    "適用停止日",
                    "基準数量",
                    "必要数量",
                    "不良率",
                    "工順",
                    1 as "階層",
                    CAST("必要数量" AS NUMERIC) as "累計数量"
                FROM "部品構成表"
                WHERE "親品目コード" = @ItemCode
                  AND ("適用停止日" IS NULL OR "適用停止日" >= CURRENT_DATE)

                UNION ALL

                -- 再帰: 子品目の子品目
                SELECT
                    b."親品目コード",
                    b."子品目コード",
                    b."適用開始日",
                    b."適用停止日",
                    b."基準数量",
                    b."必要数量",
                    b."不良率",
                    b."工順",
                    be."階層" + 1,
                    be."累計数量" * CAST(b."必要数量" AS NUMERIC)
                FROM "部品構成表" b
                INNER JOIN bom_explosion be ON b."親品目コード" = be."子品目コード"
                WHERE (b."適用停止日" IS NULL OR b."適用停止日" >= CURRENT_DATE)
                  AND be."階層" < 10  -- 無限ループ防止
            )
            SELECT
                "親品目コード" as ParentItemCode,
                "子品目コード" as ChildItemCode,
                "適用開始日" as EffectiveFrom,
                "適用停止日" as EffectiveTo,
                "基準数量" as BaseQuantity,
                "必要数量" as RequiredQuantity,
                "不良率" as DefectRate,
                "工順" as Sequence,
                "階層" as Level,
                "累計数量" * @Quantity as TotalQuantity
            FROM bom_explosion
            ORDER BY "階層", "親品目コード", "工順", "子品目コード"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<BomExplosion>(sql, new { ItemCode = itemCode, Quantity = quantity });
        return result.ToList();
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """TRUNCATE TABLE "部品構成表" CASCADE""";

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
