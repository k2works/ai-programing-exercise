using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 作業指示リポジトリ実装
/// </summary>
public class WorkOrderRepository : IWorkOrderRepository
{
    private readonly string _connectionString;

    public WorkOrderRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(WorkOrder workOrder)
    {
        const string sql = """
            INSERT INTO "作業指示データ" (
                "作業指示番号", "オーダ番号", "作業指示日", "品目コード", "作業指示数",
                "場所コード", "開始予定日", "完成予定日", "ステータス", "完了フラグ", "備考", "作成者"
            ) VALUES (
                @WorkOrderNumber, @OrderNumber, @WorkOrderDate, @ItemCode, @OrderQuantity,
                @LocationCode, @PlannedStartDate, @PlannedEndDate, @Status::作業指示ステータス, @CompletedFlag, @Remarks, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        workOrder.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            workOrder.WorkOrderNumber,
            workOrder.OrderNumber,
            workOrder.WorkOrderDate,
            workOrder.ItemCode,
            workOrder.OrderQuantity,
            workOrder.LocationCode,
            workOrder.PlannedStartDate,
            workOrder.PlannedEndDate,
            Status = workOrder.Status.GetDisplayName(),
            workOrder.CompletedFlag,
            workOrder.Remarks,
            workOrder.CreatedBy
        });
    }

    public async Task<WorkOrder?> FindByWorkOrderNumberAsync(string workOrderNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "作業指示番号" AS WorkOrderNumber,
                "オーダ番号" AS OrderNumber,
                "作業指示日" AS WorkOrderDate,
                "品目コード" AS ItemCode,
                "作業指示数" AS OrderQuantity,
                "場所コード" AS LocationCode,
                "開始予定日" AS PlannedStartDate,
                "完成予定日" AS PlannedEndDate,
                "実績開始日" AS ActualStartDate,
                "実績完了日" AS ActualEndDate,
                "完成済数" AS CompletedQuantity,
                "総良品数" AS TotalGoodQuantity,
                "総不良品数" AS TotalDefectQuantity,
                "ステータス"::TEXT AS StatusValue,
                "完了フラグ" AS CompletedFlag,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "作成者" AS CreatedBy,
                "更新日時" AS UpdatedAt,
                "更新者" AS UpdatedBy
            FROM "作業指示データ"
            WHERE "作業指示番号" = @WorkOrderNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<WorkOrder>(sql, new { WorkOrderNumber = workOrderNumber });
    }

    public async Task<string?> FindLatestWorkOrderNumberAsync(string prefix)
    {
        const string sql = """
            SELECT "作業指示番号" FROM "作業指示データ"
            WHERE "作業指示番号" LIKE @Prefix
            ORDER BY "作業指示番号" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<string>(sql, new { Prefix = prefix });
    }

    public async Task StartWorkAsync(string workOrderNumber, DateOnly actualStartDate)
    {
        const string sql = """
            UPDATE "作業指示データ"
            SET "ステータス" = '作業中'::作業指示ステータス,
                "実績開始日" = @ActualStartDate,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "作業指示番号" = @WorkOrderNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new { WorkOrderNumber = workOrderNumber, ActualStartDate = actualStartDate });
    }

    public async Task CompleteWorkAsync(string workOrderNumber, DateOnly actualEndDate)
    {
        const string sql = """
            UPDATE "作業指示データ"
            SET "ステータス" = '完了'::作業指示ステータス,
                "完了フラグ" = true,
                "実績完了日" = @ActualEndDate,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "作業指示番号" = @WorkOrderNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new { WorkOrderNumber = workOrderNumber, ActualEndDate = actualEndDate });
    }

    public async Task UpdateCompletionQuantitiesAsync(string workOrderNumber, decimal completedQuantity, decimal goodQuantity, decimal defectQuantity)
    {
        const string sql = """
            UPDATE "作業指示データ"
            SET "完成済数" = "完成済数" + @CompletedQuantity,
                "総良品数" = "総良品数" + @GoodQuantity,
                "総不良品数" = "総不良品数" + @DefectQuantity,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "作業指示番号" = @WorkOrderNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            WorkOrderNumber = workOrderNumber,
            CompletedQuantity = completedQuantity,
            GoodQuantity = goodQuantity,
            DefectQuantity = defectQuantity
        });
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync("""DELETE FROM "作業指示データ" """);
    }
}
