using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 基準生産計画リポジトリ実装
/// </summary>
public class MpsRepository : IMpsRepository
{
    private readonly string _connectionString;

    static MpsRepository()
    {
        SqlMapper.AddTypeHandler(new PlanStatusTypeHandler());
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public MpsRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(MasterProductionSchedule mps)
    {
        const string sql = """
            INSERT INTO "基準生産計画" (
                "MPS番号", "計画日", "品目コード", "計画数量", "納期",
                "ステータス", "場所コード", "備考", "作成者"
            ) VALUES (
                @MpsNumber, @PlanDate, @ItemCode, @PlanQuantity, @DueDate,
                @Status::計画ステータス, @LocationCode, @Remarks, @CreatedBy
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        mps.Id = await connection.ExecuteScalarAsync<int>(sql, new
        {
            mps.MpsNumber,
            mps.PlanDate,
            mps.ItemCode,
            mps.PlanQuantity,
            mps.DueDate,
            Status = mps.Status.GetDisplayName(),
            mps.LocationCode,
            mps.Remarks,
            mps.CreatedBy
        });
    }

    public async Task<MasterProductionSchedule?> FindByIdAsync(int id)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "MPS番号" as MpsNumber,
                "計画日" as PlanDate,
                "品目コード" as ItemCode,
                "計画数量" as PlanQuantity,
                "納期" as DueDate,
                "ステータス"::TEXT as StatusValue,
                "場所コード" as LocationCode,
                "備考" as Remarks,
                "作成日時" as CreatedAt,
                "作成者" as CreatedBy,
                "更新日時" as UpdatedAt,
                "更新者" as UpdatedBy
            FROM "基準生産計画"
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<MasterProductionSchedule>(sql, new { Id = id });
    }

    public async Task<MasterProductionSchedule?> FindByMpsNumberAsync(string mpsNumber)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "MPS番号" as MpsNumber,
                "計画日" as PlanDate,
                "品目コード" as ItemCode,
                "計画数量" as PlanQuantity,
                "納期" as DueDate,
                "ステータス"::TEXT as StatusValue,
                "場所コード" as LocationCode,
                "備考" as Remarks,
                "作成日時" as CreatedAt,
                "作成者" as CreatedBy,
                "更新日時" as UpdatedAt,
                "更新者" as UpdatedBy
            FROM "基準生産計画"
            WHERE "MPS番号" = @MpsNumber
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<MasterProductionSchedule>(sql, new { MpsNumber = mpsNumber });
    }

    public async Task<IReadOnlyList<MasterProductionSchedule>> FindByStatusAsync(PlanStatus status)
    {
        const string sql = """
            SELECT
                "ID" as Id,
                "MPS番号" as MpsNumber,
                "計画日" as PlanDate,
                "品目コード" as ItemCode,
                "計画数量" as PlanQuantity,
                "納期" as DueDate,
                "ステータス"::TEXT as StatusValue,
                "場所コード" as LocationCode,
                "備考" as Remarks,
                "作成日時" as CreatedAt,
                "作成者" as CreatedBy,
                "更新日時" as UpdatedAt,
                "更新者" as UpdatedBy
            FROM "基準生産計画"
            WHERE "ステータス" = @Status::計画ステータス
            ORDER BY "納期"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<MasterProductionSchedule>(sql, new { Status = status.GetDisplayName() });
        return result.ToList();
    }

    public async Task UpdateStatusAsync(int id, PlanStatus status)
    {
        const string sql = """
            UPDATE "基準生産計画"
            SET "ステータス" = @Status::計画ステータス,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "ID" = @Id
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new { Id = id, Status = status.GetDisplayName() });
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "基準生産計画" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
