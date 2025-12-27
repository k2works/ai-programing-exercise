using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Subcontract;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 支給明細リポジトリ実装
/// </summary>
public class SupplyDetailRepository : ISupplyDetailRepository
{
    private readonly string _connectionString;

    static SupplyDetailRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public SupplyDetailRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(SupplyDetail detail)
    {
        const string sql = """
            INSERT INTO "支給明細データ" (
                "支給番号", "支給行番号", "品目コード", "支給数", "支給単価", "支給金額", "備考"
            ) VALUES (
                @SupplyNumber, @LineNumber, @ItemCode, @Quantity, @UnitPrice, @Amount, @Remarks
            )
            RETURNING "ID"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        detail.Id = await connection.ExecuteScalarAsync<int>(sql, detail);
    }

    public async Task<IReadOnlyList<SupplyDetail>> FindBySupplyNumberAsync(string supplyNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "支給番号" AS SupplyNumber,
                "支給行番号" AS LineNumber,
                "品目コード" AS ItemCode,
                "支給数" AS Quantity,
                "支給単価" AS UnitPrice,
                "支給金額" AS Amount,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "支給明細データ"
            WHERE "支給番号" = @SupplyNumber
            ORDER BY "支給行番号"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var results = await connection.QueryAsync<SupplyDetail>(
            sql, new { SupplyNumber = supplyNumber });
        return results.ToList();
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """DELETE FROM "支給明細データ" """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
