using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Cost;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 原価リポジトリ
/// </summary>
public class CostRepository : ICostRepository
{
    private readonly string _connectionString;

    static CostRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public CostRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    private NpgsqlConnection CreateConnection() => new(_connectionString);

    public async Task<StandardCost?> FindStandardCostByItemCodeAsync(string itemCode, DateOnly targetDate)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "品目コード" AS ItemCode,
                "適用開始日" AS EffectiveStartDate,
                "適用終了日" AS EffectiveEndDate,
                "標準材料費" AS StandardMaterialCost,
                "標準労務費" AS StandardLaborCost,
                "標準経費" AS StandardExpense,
                "標準製造原価" AS StandardManufacturingCost,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "標準原価マスタ"
            WHERE "品目コード" = @ItemCode
              AND "適用開始日" <= @TargetDate
              AND ("適用終了日" IS NULL OR "適用終了日" >= @TargetDate)
            ORDER BY "適用開始日" DESC
            LIMIT 1
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<StandardCost>(sql, new { ItemCode = itemCode, TargetDate = targetDate });
    }

    public async Task<IReadOnlyList<StandardCost>> FindAllStandardCostsByItemCodeAsync(string itemCode)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "品目コード" AS ItemCode,
                "適用開始日" AS EffectiveStartDate,
                "適用終了日" AS EffectiveEndDate,
                "標準材料費" AS StandardMaterialCost,
                "標準労務費" AS StandardLaborCost,
                "標準経費" AS StandardExpense,
                "標準製造原価" AS StandardManufacturingCost,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "標準原価マスタ"
            WHERE "品目コード" = @ItemCode
            ORDER BY "適用開始日" DESC
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<StandardCost>(sql, new { ItemCode = itemCode });
        return result.ToList();
    }

    public async Task<ActualCost?> FindActualCostByWorkOrderNumberAsync(string workOrderNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "作業指示番号" AS WorkOrderNumber,
                "品目コード" AS ItemCode,
                "完成数量" AS CompletedQuantity,
                "実際材料費" AS ActualMaterialCost,
                "実際労務費" AS ActualLaborCost,
                "実際経費" AS ActualExpense,
                "実際製造原価" AS ActualManufacturingCost,
                "単位原価" AS UnitCost,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "実際原価データ"
            WHERE "作業指示番号" = @WorkOrderNumber
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<ActualCost>(sql, new { WorkOrderNumber = workOrderNumber });
    }

    public async Task<CostVariance?> FindCostVarianceByWorkOrderNumberAsync(string workOrderNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "作業指示番号" AS WorkOrderNumber,
                "品目コード" AS ItemCode,
                "材料費差異" AS MaterialCostVariance,
                "労務費差異" AS LaborCostVariance,
                "経費差異" AS ExpenseVariance,
                "総差異" AS TotalVariance,
                "作成日時" AS CreatedAt
            FROM "原価差異データ"
            WHERE "作業指示番号" = @WorkOrderNumber
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<CostVariance>(sql, new { WorkOrderNumber = workOrderNumber });
    }

    public async Task<long> SaveStandardCostAsync(StandardCost standardCost)
    {
        const string sql = """
            INSERT INTO "標準原価マスタ" (
                "品目コード", "適用開始日", "適用終了日",
                "標準材料費", "標準労務費", "標準経費", "標準製造原価"
            ) VALUES (
                @ItemCode, @EffectiveStartDate, @EffectiveEndDate,
                @StandardMaterialCost, @StandardLaborCost, @StandardExpense, @StandardManufacturingCost
            )
            RETURNING "ID"
            """;

        await using var connection = CreateConnection();
        return await connection.ExecuteScalarAsync<long>(sql, standardCost);
    }

    public async Task<long> SaveActualCostAsync(ActualCost actualCost)
    {
        const string sql = """
            INSERT INTO "実際原価データ" (
                "作業指示番号", "品目コード", "完成数量",
                "実際材料費", "実際労務費", "実際経費",
                "実際製造原価", "単位原価"
            ) VALUES (
                @WorkOrderNumber, @ItemCode, @CompletedQuantity,
                @ActualMaterialCost, @ActualLaborCost, @ActualExpense,
                @ActualManufacturingCost, @UnitCost
            )
            RETURNING "ID"
            """;

        await using var connection = CreateConnection();
        return await connection.ExecuteScalarAsync<long>(sql, actualCost);
    }

    public async Task<long> SaveCostVarianceAsync(CostVariance variance)
    {
        const string sql = """
            INSERT INTO "原価差異データ" (
                "作業指示番号", "品目コード",
                "材料費差異", "労務費差異", "経費差異", "総差異"
            ) VALUES (
                @WorkOrderNumber, @ItemCode,
                @MaterialCostVariance, @LaborCostVariance,
                @ExpenseVariance, @TotalVariance
            )
            RETURNING "ID"
            """;

        await using var connection = CreateConnection();
        return await connection.ExecuteScalarAsync<long>(sql, variance);
    }

    public async Task DeleteAllCostVariancesAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "原価差異データ" """);
    }

    public async Task DeleteAllActualCostsAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "実際原価データ" """);
    }

    public async Task DeleteAllStandardCostsAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "標準原価マスタ" """);
    }
}
