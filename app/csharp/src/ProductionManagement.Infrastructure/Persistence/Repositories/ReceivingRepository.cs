using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 入荷受入リポジトリ実装
/// </summary>
public class ReceivingRepository : IReceivingRepository
{
    private readonly string _connectionString;

    static ReceivingRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
        SqlMapper.AddTypeHandler(new ReceivingTypeTypeHandler());
    }

    public ReceivingRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Receiving receiving)
    {
        const string sql = """
            INSERT INTO "入荷受入データ" (
                "入荷番号", "発注番号", "発注行番号", "入荷日", "品目コード",
                "入荷数量", "入荷区分", "納品書番号", "ロット番号", "入荷場所コード",
                "備考", "作成者"
            ) VALUES (
                @ReceivingNumber, @PurchaseOrderNumber, @LineNumber, @ReceivingDate, @ItemCode,
                @ReceivingQuantity, @ReceivingType::入荷受入区分, @DeliveryNoteNumber, @LotNumber, @ReceivingLocationCode,
                @Remarks, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        receiving.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            receiving.ReceivingNumber,
            receiving.PurchaseOrderNumber,
            receiving.LineNumber,
            receiving.ReceivingDate,
            receiving.ItemCode,
            receiving.ReceivingQuantity,
            ReceivingType = receiving.ReceivingType.GetDisplayName(),
            receiving.DeliveryNoteNumber,
            receiving.LotNumber,
            receiving.ReceivingLocationCode,
            receiving.Remarks,
            receiving.CreatedBy
        });
    }

    public async Task<Receiving?> FindByIdAsync(int id)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "入荷番号" AS ReceivingNumber,
                "発注番号" AS PurchaseOrderNumber,
                "発注行番号" AS LineNumber,
                "入荷日" AS ReceivingDate,
                "品目コード" AS ItemCode,
                "入荷数量" AS ReceivingQuantity,
                "入荷区分"::TEXT AS ReceivingTypeValue,
                "納品書番号" AS DeliveryNoteNumber,
                "ロット番号" AS LotNumber,
                "入荷場所コード" AS ReceivingLocationCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "入荷受入データ"
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Receiving>(sql, new { Id = id });
    }

    public async Task<Receiving?> FindByReceivingNumberAsync(string receivingNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "入荷番号" AS ReceivingNumber,
                "発注番号" AS PurchaseOrderNumber,
                "発注行番号" AS LineNumber,
                "入荷日" AS ReceivingDate,
                "品目コード" AS ItemCode,
                "入荷数量" AS ReceivingQuantity,
                "入荷区分"::TEXT AS ReceivingTypeValue,
                "納品書番号" AS DeliveryNoteNumber,
                "ロット番号" AS LotNumber,
                "入荷場所コード" AS ReceivingLocationCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "入荷受入データ"
            WHERE "入荷番号" = @ReceivingNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<Receiving>(
            sql, new { ReceivingNumber = receivingNumber });
    }

    public async Task<IReadOnlyList<Receiving>> FindByPurchaseOrderNumberAsync(string purchaseOrderNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "入荷番号" AS ReceivingNumber,
                "発注番号" AS PurchaseOrderNumber,
                "発注行番号" AS LineNumber,
                "入荷日" AS ReceivingDate,
                "品目コード" AS ItemCode,
                "入荷数量" AS ReceivingQuantity,
                "入荷区分"::TEXT AS ReceivingTypeValue,
                "納品書番号" AS DeliveryNoteNumber,
                "ロット番号" AS LotNumber,
                "入荷場所コード" AS ReceivingLocationCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "入荷受入データ"
            WHERE "発注番号" = @PurchaseOrderNumber
            ORDER BY "入荷番号"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var results = await connection.QueryAsync<Receiving>(
            sql, new { PurchaseOrderNumber = purchaseOrderNumber });
        return results.ToList();
    }

    public async Task<string?> FindLatestReceivingNumberAsync(string prefix)
    {
        const string sql = """
            SELECT "入荷番号"
            FROM "入荷受入データ"
            WHERE "入荷番号" LIKE @Prefix
            ORDER BY "入荷番号" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<string>(sql, new { Prefix = prefix });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "入荷受入データ" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
