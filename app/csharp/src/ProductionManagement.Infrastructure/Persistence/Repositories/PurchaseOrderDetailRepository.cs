using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 発注明細リポジトリ実装
/// </summary>
public class PurchaseOrderDetailRepository : IPurchaseOrderDetailRepository
{
    private readonly string _connectionString;

    static PurchaseOrderDetailRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public PurchaseOrderDetailRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(PurchaseOrderDetail detail)
    {
        const string sql = """
            INSERT INTO "発注明細データ" (
                "発注番号", "発注行番号", "オーダNO", "納入場所コード", "品目コード",
                "諸口品目区分", "受入予定日", "回答納期", "発注単価", "発注数量",
                "発注金額", "消費税金額", "作成者"
            ) VALUES (
                @PurchaseOrderNumber, @LineNumber, @OrderNumber, @DeliveryLocationCode, @ItemCode,
                @MiscellaneousItemFlag, @ExpectedReceivingDate, @ConfirmedDeliveryDate, @OrderUnitPrice, @OrderQuantity,
                @OrderAmount, @TaxAmount, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        detail.Id = await connection.ExecuteScalarAsync<int>(sql, detail);
    }

    public async Task<IReadOnlyList<PurchaseOrderDetail>> FindByPurchaseOrderNumberAsync(
        string purchaseOrderNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "発注番号" AS PurchaseOrderNumber,
                "発注行番号" AS LineNumber,
                "オーダNO" AS OrderNumber,
                "納入場所コード" AS DeliveryLocationCode,
                "品目コード" AS ItemCode,
                "諸口品目区分" AS MiscellaneousItemFlag,
                "受入予定日" AS ExpectedReceivingDate,
                "回答納期" AS ConfirmedDeliveryDate,
                "発注単価" AS OrderUnitPrice,
                "発注数量" AS OrderQuantity,
                "入荷済数量" AS ReceivedQuantity,
                "検査済数量" AS InspectedQuantity,
                "検収済数量" AS AcceptedQuantity,
                "発注金額" AS OrderAmount,
                "消費税金額" AS TaxAmount,
                "完了フラグ" AS CompletedFlag,
                "明細備考" AS DetailRemarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "発注明細データ"
            WHERE "発注番号" = @PurchaseOrderNumber
            ORDER BY "発注行番号"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var results = await connection.QueryAsync<PurchaseOrderDetail>(
            sql, new { PurchaseOrderNumber = purchaseOrderNumber });
        return results.ToList();
    }

    public async Task<PurchaseOrderDetail?> FindByPurchaseOrderNumberAndLineNumberAsync(
        string purchaseOrderNumber, int lineNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "発注番号" AS PurchaseOrderNumber,
                "発注行番号" AS LineNumber,
                "オーダNO" AS OrderNumber,
                "納入場所コード" AS DeliveryLocationCode,
                "品目コード" AS ItemCode,
                "諸口品目区分" AS MiscellaneousItemFlag,
                "受入予定日" AS ExpectedReceivingDate,
                "回答納期" AS ConfirmedDeliveryDate,
                "発注単価" AS OrderUnitPrice,
                "発注数量" AS OrderQuantity,
                "入荷済数量" AS ReceivedQuantity,
                "検査済数量" AS InspectedQuantity,
                "検収済数量" AS AcceptedQuantity,
                "発注金額" AS OrderAmount,
                "消費税金額" AS TaxAmount,
                "完了フラグ" AS CompletedFlag,
                "明細備考" AS DetailRemarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "発注明細データ"
            WHERE "発注番号" = @PurchaseOrderNumber AND "発注行番号" = @LineNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<PurchaseOrderDetail>(
            sql, new { PurchaseOrderNumber = purchaseOrderNumber, LineNumber = lineNumber });
    }

    public async Task UpdateReceivedQuantityAsync(
        string purchaseOrderNumber, int lineNumber, decimal receivedQuantity)
    {
        const string sql = """
            UPDATE "発注明細データ"
            SET "入荷済数量" = @ReceivedQuantity, "更新日時" = CURRENT_TIMESTAMP
            WHERE "発注番号" = @PurchaseOrderNumber AND "発注行番号" = @LineNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            PurchaseOrderNumber = purchaseOrderNumber,
            LineNumber = lineNumber,
            ReceivedQuantity = receivedQuantity
        });
    }

    public async Task UpdateInspectedQuantityAsync(
        string purchaseOrderNumber, int lineNumber, decimal inspectedQuantity)
    {
        const string sql = """
            UPDATE "発注明細データ"
            SET "検査済数量" = @InspectedQuantity, "更新日時" = CURRENT_TIMESTAMP
            WHERE "発注番号" = @PurchaseOrderNumber AND "発注行番号" = @LineNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            PurchaseOrderNumber = purchaseOrderNumber,
            LineNumber = lineNumber,
            InspectedQuantity = inspectedQuantity
        });
    }

    public async Task UpdateAcceptedQuantityAsync(
        string purchaseOrderNumber, int lineNumber, decimal acceptedQuantity)
    {
        const string sql = """
            UPDATE "発注明細データ"
            SET "検収済数量" = @AcceptedQuantity, "更新日時" = CURRENT_TIMESTAMP
            WHERE "発注番号" = @PurchaseOrderNumber AND "発注行番号" = @LineNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            PurchaseOrderNumber = purchaseOrderNumber,
            LineNumber = lineNumber,
            AcceptedQuantity = acceptedQuantity
        });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "発注明細データ" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
