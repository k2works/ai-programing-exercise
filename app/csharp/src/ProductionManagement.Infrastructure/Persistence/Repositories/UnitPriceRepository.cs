using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 単価マスタリポジトリ実装
/// </summary>
public class UnitPriceRepository : IUnitPriceRepository
{
    private readonly string _connectionString;

    static UnitPriceRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public UnitPriceRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(UnitPrice unitPrice)
    {
        const string sql = """
            INSERT INTO "単価マスタ" (
                "品目コード", "取引先コード", "ロット単位数", "使用開始日",
                "使用停止日", "単価", "作成者"
            ) VALUES (
                @ItemCode, @SupplierCode, @LotUnitQuantity, @EffectiveFrom,
                @EffectiveTo, @Price, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        unitPrice.Id = await connection.ExecuteScalarAsync<int>(sql, unitPrice);
    }

    public async Task<UnitPrice?> FindEffectiveUnitPriceAsync(
        string itemCode, string supplierCode, DateOnly date)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "品目コード" AS ItemCode,
                "取引先コード" AS SupplierCode,
                "ロット単位数" AS LotUnitQuantity,
                "使用開始日" AS EffectiveFrom,
                "使用停止日" AS EffectiveTo,
                "単価" AS Price,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "単価マスタ"
            WHERE "品目コード" = @ItemCode
              AND "取引先コード" = @SupplierCode
              AND "使用開始日" <= @Date
              AND ("使用停止日" IS NULL OR "使用停止日" >= @Date)
            ORDER BY "使用開始日" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<UnitPrice>(
            sql, new { ItemCode = itemCode, SupplierCode = supplierCode, Date = date });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "単価マスタ" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
