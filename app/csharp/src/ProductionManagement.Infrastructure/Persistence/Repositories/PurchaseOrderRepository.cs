using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 発注リポジトリ実装
/// </summary>
public class PurchaseOrderRepository : IPurchaseOrderRepository
{
    private readonly string _connectionString;

    static PurchaseOrderRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
        SqlMapper.AddTypeHandler(new PurchaseOrderStatusTypeHandler());
    }

    public PurchaseOrderRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(PurchaseOrder purchaseOrder)
    {
        const string sql = """
            INSERT INTO "発注データ" (
                "発注番号", "発注日", "取引先コード", "発注担当者コード",
                "発注部門コード", "ステータス", "備考", "作成者"
            ) VALUES (
                @PurchaseOrderNumber, @OrderDate, @SupplierCode, @OrdererCode,
                @DepartmentCode, @Status::発注ステータス, @Remarks, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        purchaseOrder.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            purchaseOrder.PurchaseOrderNumber,
            purchaseOrder.OrderDate,
            purchaseOrder.SupplierCode,
            purchaseOrder.OrdererCode,
            purchaseOrder.DepartmentCode,
            Status = purchaseOrder.Status.GetDisplayName(),
            purchaseOrder.Remarks,
            purchaseOrder.CreatedBy
        });
    }

    public async Task<PurchaseOrder?> FindByIdAsync(int id)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "発注番号" AS PurchaseOrderNumber,
                "発注日" AS OrderDate,
                "取引先コード" AS SupplierCode,
                "発注担当者コード" AS OrdererCode,
                "発注部門コード" AS DepartmentCode,
                "ステータス"::TEXT AS StatusValue,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "発注データ"
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<PurchaseOrder>(sql, new { Id = id });
    }

    public async Task<PurchaseOrder?> FindByPurchaseOrderNumberAsync(string purchaseOrderNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "発注番号" AS PurchaseOrderNumber,
                "発注日" AS OrderDate,
                "取引先コード" AS SupplierCode,
                "発注担当者コード" AS OrdererCode,
                "発注部門コード" AS DepartmentCode,
                "ステータス"::TEXT AS StatusValue,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "発注データ"
            WHERE "発注番号" = @PurchaseOrderNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<PurchaseOrder>(
            sql, new { PurchaseOrderNumber = purchaseOrderNumber });
    }

    public async Task<string?> FindLatestPurchaseOrderNumberAsync(string prefix)
    {
        const string sql = """
            SELECT "発注番号"
            FROM "発注データ"
            WHERE "発注番号" LIKE @Prefix
            ORDER BY "発注番号" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<string>(sql, new { Prefix = prefix });
    }

    public async Task UpdateStatusAsync(string purchaseOrderNumber, PurchaseOrderStatus status)
    {
        const string sql = """
            UPDATE "発注データ"
            SET "ステータス" = @Status::発注ステータス, "更新日時" = CURRENT_TIMESTAMP
            WHERE "発注番号" = @PurchaseOrderNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            PurchaseOrderNumber = purchaseOrderNumber,
            Status = status.GetDisplayName()
        });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "発注データ" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
