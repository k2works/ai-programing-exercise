using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 引当情報リポジトリ実装
/// </summary>
public class AllocationRepository : IAllocationRepository
{
    private readonly string _connectionString;

    static AllocationRepository()
    {
        SqlMapper.AddTypeHandler(new AllocationTypeTypeHandler());
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public AllocationRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Allocation allocation)
    {
        const string sql = """
            INSERT INTO "引当情報" (
                "所要ID", "引当区分", "オーダID", "引当日", "引当数量", "場所コード"
            ) VALUES (
                @RequirementId, @AllocationType::引当区分, @OrderId, @AllocationDate, @AllocatedQuantity, @LocationCode
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        allocation.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            allocation.RequirementId,
            AllocationType = allocation.AllocationType.GetDisplayName(),
            allocation.OrderId,
            allocation.AllocationDate,
            allocation.AllocatedQuantity,
            allocation.LocationCode
        });
    }

    public async Task<IReadOnlyList<Allocation>> FindByRequirementIdAsync(int requirementId)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "所要ID" as RequirementId,
                "引当区分"::TEXT as AllocationTypeValue,
                "オーダID" as OrderId,
                "引当日" as AllocationDate,
                "引当数量" as AllocatedQuantity,
                "場所コード" as LocationCode,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "引当情報"
            WHERE "所要ID" = @RequirementId
            ORDER BY "引当日"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Allocation>(sql, new { RequirementId = requirementId });
        return result.ToList();
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "引当情報" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
