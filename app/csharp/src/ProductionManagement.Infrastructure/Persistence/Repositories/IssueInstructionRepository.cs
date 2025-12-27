using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 払出指示リポジトリ
/// </summary>
public class IssueInstructionRepository : IIssueInstructionRepository
{
    private readonly string _connectionString;

    public IssueInstructionRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    private NpgsqlConnection CreateConnection() => new(_connectionString);

    public async Task<IssueInstruction?> FindByInstructionNumberAsync(string instructionNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "払出指示番号" AS InstructionNumber,
                "オーダ番号" AS OrderNumber,
                "払出指示日" AS InstructionDate,
                "場所コード" AS LocationCode,
                "備考" AS Remarks,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "払出指示データ"
            WHERE "払出指示番号" = @InstructionNumber
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<IssueInstruction>(sql, new { InstructionNumber = instructionNumber });
    }

    public async Task<IReadOnlyList<IssueInstructionDetail>> FindDetailsByInstructionNumberAsync(string instructionNumber)
    {
        const string sql = """
            SELECT
                "ID" AS Id,
                "払出指示番号" AS InstructionNumber,
                "払出行番号" AS LineNumber,
                "品目コード" AS ItemCode,
                "工順" AS RoutingSequence,
                "払出数量" AS IssueQuantity,
                "作成日時" AS CreatedAt,
                "更新日時" AS UpdatedAt
            FROM "払出指示明細データ"
            WHERE "払出指示番号" = @InstructionNumber
            ORDER BY "払出行番号"
            """;

        await using var connection = CreateConnection();
        var result = await connection.QueryAsync<IssueInstructionDetail>(sql, new { InstructionNumber = instructionNumber });
        return result.ToList();
    }

    public async Task<string?> FindLatestInstructionNumberAsync(string pattern)
    {
        const string sql = """
            SELECT "払出指示番号"
            FROM "払出指示データ"
            WHERE "払出指示番号" LIKE @Pattern
            ORDER BY "払出指示番号" DESC
            LIMIT 1
            """;

        await using var connection = CreateConnection();
        return await connection.QuerySingleOrDefaultAsync<string>(sql, new { Pattern = pattern });
    }

    public async Task<long> SaveAsync(IssueInstruction instruction)
    {
        const string sql = """
            INSERT INTO "払出指示データ" (
                "払出指示番号", "オーダ番号", "払出指示日", "場所コード", "備考"
            ) VALUES (
                @InstructionNumber, @OrderNumber, @InstructionDate, @LocationCode, @Remarks
            )
            RETURNING "ID"
            """;

        await using var connection = CreateConnection();
        return await connection.ExecuteScalarAsync<long>(sql, instruction);
    }

    public async Task SaveDetailAsync(IssueInstructionDetail detail)
    {
        const string sql = """
            INSERT INTO "払出指示明細データ" (
                "払出指示番号", "払出行番号", "品目コード", "工順", "払出数量"
            ) VALUES (
                @InstructionNumber, @LineNumber, @ItemCode, @RoutingSequence, @IssueQuantity
            )
            """;

        await using var connection = CreateConnection();
        await connection.ExecuteAsync(sql, detail);
    }

    public async Task DeleteAllAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "払出指示データ" """);
    }

    public async Task DeleteAllDetailsAsync()
    {
        await using var connection = CreateConnection();
        await connection.ExecuteAsync("""DELETE FROM "払出指示明細データ" """);
    }
}
