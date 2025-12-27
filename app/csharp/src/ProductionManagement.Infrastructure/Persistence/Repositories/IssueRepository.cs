using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 払出リポジトリ
/// </summary>
public class IssueRepository : IIssueRepository
{
    private readonly string _connectionString;

    public IssueRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    private NpgsqlConnection CreateConnection() => new(_connectionString);

    public async Task<Issue?> FindByIssueNumberAsync(string issueNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "払出番号" AS IssueNumber,
                "作業指示番号" AS WorkOrderNumber,
                "工順" AS RoutingSequence,
                "場所コード" AS LocationCode,
                "払出日" AS IssueDate,
                "払出担当者コード" AS IssuerCode,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "払出データ"
            WHERE "払出番号" = @IssueNumber
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<Issue>(sql, new { IssueNumber = issueNumber });
    }

    public async Task<IReadOnlyList<IssueDetail>> FindDetailsByIssueNumberAsync(string issueNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "払出番号" AS IssueNumber,
                "払出行番号" AS LineNumber,
                "品目コード" AS ItemCode,
                "払出数" AS IssueQuantity,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "払出明細データ"
            WHERE "払出番号" = @IssueNumber
            ORDER BY "払出行番号"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<IssueDetail>(sql, new { IssueNumber = issueNumber });
        return result.ToList();
    }

    public async Task<IReadOnlyList<Issue>> FindByWorkOrderNumberAsync(string workOrderNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "払出番号" AS IssueNumber,
                "作業指示番号" AS WorkOrderNumber,
                "工順" AS RoutingSequence,
                "場所コード" AS LocationCode,
                "払出日" AS IssueDate,
                "払出担当者コード" AS IssuerCode,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "払出データ"
            WHERE "作業指示番号" = @WorkOrderNumber
            ORDER BY "払出日", "払出番号"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<Issue>(sql, new { WorkOrderNumber = workOrderNumber });
        return result.ToList();
    }

    public async Task<string?> FindLatestIssueNumberAsync(string pattern)
    {
        const string sql = """
            SELECT "払出番号"
            FROM "払出データ"
            WHERE "払出番号" LIKE @Pattern
            ORDER BY "払出番号" DESC
            LIMIT 1
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<string>(sql, new { Pattern = pattern });
    }

    public async Task<long> SaveAsync(Issue issue)
    {
        const string sql = """
            INSERT INTO "払出データ" (
                "払出番号", "作業指示番号", "工順", "場所コード", "払出日", "払出担当者コード"
            ) VALUES (
                @IssueNumber, @WorkOrderNumber, @RoutingSequence, @LocationCode, @IssueDate, @IssuerCode
            )
            RETURNING "ID"
            """;

        await using var connection = CreateConnection();
        return await connection.ExecuteScalarAsync<long>(sql, issue);
    }

    public async Task SaveDetailAsync(IssueDetail detail)
    {
        const string sql = """
            INSERT INTO "払出明細データ" (
                "払出番号", "払出行番号", "品目コード", "払出数"
            ) VALUES (
                @IssueNumber, @LineNumber, @ItemCode, @IssueQuantity
            )
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, detail);
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "払出データ" """);
    }

    public async Task DeleteAllDetailsAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "払出明細データ" """);
    }
}
