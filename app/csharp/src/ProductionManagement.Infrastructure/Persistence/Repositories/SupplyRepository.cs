using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Subcontract;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 支給リポジトリ実装
/// </summary>
public class SupplyRepository : ISupplyRepository
{
    private readonly string _connectionString;

    static SupplyRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
        SqlMapper.AddTypeHandler(new SupplyTypeTypeHandler());
    }

    public SupplyRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Supply supply)
    {
        const string sql = """
            INSERT INTO "支給データ" (
                "支給番号", "発注番号", "発注行番号", "取引先コード", "支給日",
                "支給担当者コード", "支給区分", "備考", "作成者"
            ) VALUES (
                @SupplyNumber, @PurchaseOrderNumber, @LineNumber, @SupplierCode, @SupplyDate,
                @SupplierPersonCode, @SupplyType::支給区分, @Remarks, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        supply.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            supply.SupplyNumber,
            supply.PurchaseOrderNumber,
            supply.LineNumber,
            supply.SupplierCode,
            supply.SupplyDate,
            supply.SupplierPersonCode,
            SupplyType = supply.SupplyType.GetDisplayName(),
            supply.Remarks,
            supply.CreatedBy
        });
    }

    public async Task<Supply?> FindByIdAsync(int id)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "支給番号" AS SupplyNumber,
                "発注番号" AS PurchaseOrderNumber,
                "発注行番号" AS LineNumber,
                "取引先コード" AS SupplierCode,
                "支給日" AS SupplyDate,
                "支給担当者コード" AS SupplierPersonCode,
                "支給区分"::TEXT AS SupplyTypeValue,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "支給データ"
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Supply>(sql, new { Id = id });
    }

    public async Task<Supply?> FindBySupplyNumberAsync(string supplyNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "支給番号" AS SupplyNumber,
                "発注番号" AS PurchaseOrderNumber,
                "発注行番号" AS LineNumber,
                "取引先コード" AS SupplierCode,
                "支給日" AS SupplyDate,
                "支給担当者コード" AS SupplierPersonCode,
                "支給区分"::TEXT AS SupplyTypeValue,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "支給データ"
            WHERE "支給番号" = @SupplyNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Supply>(
            sql, new { SupplyNumber = supplyNumber });
    }

    public async Task<IReadOnlyList<Supply>> FindByPurchaseOrderDetailAsync(
        string purchaseOrderNumber, int lineNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "支給番号" AS SupplyNumber,
                "発注番号" AS PurchaseOrderNumber,
                "発注行番号" AS LineNumber,
                "取引先コード" AS SupplierCode,
                "支給日" AS SupplyDate,
                "支給担当者コード" AS SupplierPersonCode,
                "支給区分"::TEXT AS SupplyTypeValue,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "支給データ"
            WHERE "発注番号" = @PurchaseOrderNumber AND "発注行番号" = @LineNumber
            ORDER BY "支給番号"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var results = await connection.QueryAsync<Supply>(
            sql, new { PurchaseOrderNumber = purchaseOrderNumber, LineNumber = lineNumber });
        return results.ToList();
    }

    public async Task<string?> FindLatestSupplyNumberAsync(string prefix)
    {
        const string sql = """
            SELECT "支給番号"
            FROM "支給データ"
            WHERE "支給番号" LIKE @Prefix
            ORDER BY "支給番号" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<string>(sql, new { Prefix = prefix });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "支給データ" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
