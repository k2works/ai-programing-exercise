using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 所要情報リポジトリ実装
/// </summary>
public class RequirementRepository : IRequirementRepository
{
    private readonly string _connectionString;

    static RequirementRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public RequirementRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Requirement requirement)
    {
        const string sql = """
            INSERT INTO "所要情報" (
                "所要NO", "オーダID", "品目コード", "納期",
                "必要数量", "引当済数量", "不足数量", "場所コード"
            ) VALUES (
                @RequirementNumber, @OrderId, @ItemCode, @DueDate,
                @RequiredQuantity, @AllocatedQuantity, @ShortageQuantity, @LocationCode
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        requirement.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            requirement.RequirementNumber,
            requirement.OrderId,
            requirement.ItemCode,
            requirement.DueDate,
            requirement.RequiredQuantity,
            requirement.AllocatedQuantity,
            requirement.ShortageQuantity,
            requirement.LocationCode
        });
    }

    public async Task<Requirement?> FindByIdAsync(int id)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "所要NO" as RequirementNumber,
                "オーダID" as OrderId,
                "品目コード" as ItemCode,
                "納期" as DueDate,
                "必要数量" as RequiredQuantity,
                "引当済数量" as AllocatedQuantity,
                "不足数量" as ShortageQuantity,
                "場所コード" as LocationCode,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "所要情報"
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Requirement>(sql, new { Id = id });
    }

    public async Task<IReadOnlyList<Requirement>> FindByOrderIdAsync(int orderId)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "所要NO" as RequirementNumber,
                "オーダID" as OrderId,
                "品目コード" as ItemCode,
                "納期" as DueDate,
                "必要数量" as RequiredQuantity,
                "引当済数量" as AllocatedQuantity,
                "不足数量" as ShortageQuantity,
                "場所コード" as LocationCode,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "所要情報"
            WHERE "オーダID" = @OrderId
            ORDER BY "納期"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Requirement>(sql, new { OrderId = orderId });
        return result.ToList();
    }

    public async Task UpdateAllocationAsync(int id, decimal allocatedQuantity, decimal shortageQuantity)
    {
        const string sql = """
            UPDATE "所要情報"
            SET "引当済数量" = @AllocatedQuantity,
                "不足数量" = @ShortageQuantity,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new { Id = id, AllocatedQuantity = allocatedQuantity, ShortageQuantity = shortageQuantity });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "所要情報" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
