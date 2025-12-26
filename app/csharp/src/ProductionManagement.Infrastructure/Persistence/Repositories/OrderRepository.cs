using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// オーダ情報リポジトリ実装
/// </summary>
public class OrderRepository : IOrderRepository
{
    private readonly string _connectionString;

    static OrderRepository()
    {
        SqlMapper.AddTypeHandler(new PlanStatusTypeHandler());
        SqlMapper.AddTypeHandler(new OrderTypeTypeHandler());
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public OrderRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Order order)
    {
        const string sql = """
            INSERT INTO "オーダ情報" (
                "オーダNO", "オーダ種別", "品目コード", "着手予定日", "納期",
                "有効期限", "計画数量", "場所コード", "ステータス", "MPS_ID", "親オーダID", "作成者"
            ) VALUES (
                @OrderNumber, @OrderType::オーダ種別, @ItemCode, @StartDate, @DueDate,
                @ExpirationDate, @PlanQuantity, @LocationCode, @Status::計画ステータス, @MpsId, @ParentOrderId, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        order.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            order.OrderNumber,
            OrderType = order.OrderType.GetDisplayName(),
            order.ItemCode,
            order.StartDate,
            order.DueDate,
            order.ExpirationDate,
            order.PlanQuantity,
            order.LocationCode,
            Status = order.Status.GetDisplayName(),
            order.MpsId,
            order.ParentOrderId,
            order.CreatedBy
        });
    }

    public async Task<Order?> FindByIdAsync(int id)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "オーダNO" as OrderNumber,
                "オーダ種別"::TEXT as OrderTypeValue,
                "品目コード" as ItemCode,
                "着手予定日" as StartDate,
                "納期" as DueDate,
                "有効期限" as ExpirationDate,
                "計画数量" as PlanQuantity,
                "場所コード" as LocationCode,
                "ステータス"::TEXT as StatusValue,
                "MPS_ID" as MpsId,
                "親オーダID" as ParentOrderId,
                "作成日時" as CreatedAt,
                "作成者" as CreatedBy,
                "更新日時" as UpdatedAt,
                "更新者" as UpdatedBy
            FROM "オーダ情報"
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Order>(sql, new { Id = id });
    }

    public async Task<Order?> FindByOrderNumberAsync(string orderNumber)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "オーダNO" as OrderNumber,
                "オーダ種別"::TEXT as OrderTypeValue,
                "品目コード" as ItemCode,
                "着手予定日" as StartDate,
                "納期" as DueDate,
                "有効期限" as ExpirationDate,
                "計画数量" as PlanQuantity,
                "場所コード" as LocationCode,
                "ステータス"::TEXT as StatusValue,
                "MPS_ID" as MpsId,
                "親オーダID" as ParentOrderId,
                "作成日時" as CreatedAt,
                "作成者" as CreatedBy,
                "更新日時" as UpdatedAt,
                "更新者" as UpdatedBy
            FROM "オーダ情報"
            WHERE "オーダNO" = @OrderNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Order>(sql, new { OrderNumber = orderNumber });
    }

    public async Task<IReadOnlyList<Order>> FindByMpsIdAsync(int mpsId)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "オーダNO" as OrderNumber,
                "オーダ種別"::TEXT as OrderTypeValue,
                "品目コード" as ItemCode,
                "着手予定日" as StartDate,
                "納期" as DueDate,
                "有効期限" as ExpirationDate,
                "計画数量" as PlanQuantity,
                "場所コード" as LocationCode,
                "ステータス"::TEXT as StatusValue,
                "MPS_ID" as MpsId,
                "親オーダID" as ParentOrderId,
                "作成日時" as CreatedAt,
                "作成者" as CreatedBy,
                "更新日時" as UpdatedAt,
                "更新者" as UpdatedBy
            FROM "オーダ情報"
            WHERE "MPS_ID" = @MpsId
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Order>(sql, new { MpsId = mpsId });
        return result.ToList();
    }

    public async Task<IReadOnlyList<Order>> FindByParentOrderIdAsync(int parentOrderId)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "オーダNO" as OrderNumber,
                "オーダ種別"::TEXT as OrderTypeValue,
                "品目コード" as ItemCode,
                "着手予定日" as StartDate,
                "納期" as DueDate,
                "有効期限" as ExpirationDate,
                "計画数量" as PlanQuantity,
                "場所コード" as LocationCode,
                "ステータス"::TEXT as StatusValue,
                "MPS_ID" as MpsId,
                "親オーダID" as ParentOrderId,
                "作成日時" as CreatedAt,
                "作成者" as CreatedBy,
                "更新日時" as UpdatedAt,
                "更新者" as UpdatedBy
            FROM "オーダ情報"
            WHERE "親オーダID" = @ParentOrderId
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Order>(sql, new { ParentOrderId = parentOrderId });
        return result.ToList();
    }

    public async Task UpdateParentOrderIdAsync(int id, int parentOrderId)
    {
        const string sql = """
            UPDATE "オーダ情報"
            SET "親オーダID" = @ParentOrderId, "更新日時" = CURRENT_TIMESTAMP
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new { Id = id, ParentOrderId = parentOrderId });
    }

    public async Task UpdateStatusAsync(int id, PlanStatus status)
    {
        const string sql = """
            UPDATE "オーダ情報"
            SET "ステータス" = @Status::計画ステータス, "更新日時" = CURRENT_TIMESTAMP
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new { Id = id, Status = status.GetDisplayName() });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "オーダ情報" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
