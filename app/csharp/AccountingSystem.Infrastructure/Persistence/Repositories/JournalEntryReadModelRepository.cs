namespace AccountingSystem.Infrastructure.Persistence.Repositories;

using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Entities;
using Dapper;
using Npgsql;

/// <summary>
/// 仕訳 Read Model リポジトリ実装
/// CQRS パターンの Query Side 用
/// </summary>
public class JournalEntryReadModelRepository : IJournalEntryReadModelRepository
{
    private readonly string _connectionString;

    public JournalEntryReadModelRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task InsertJournalEntryAsync(
        string id,
        DateOnly entryDate,
        string description,
        string status,
        bool deleted,
        DateTime createdAt,
        DateTime updatedAt,
        string? approvedBy,
        string? approvalComment)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            INSERT INTO ""仕訳ReadModel"" (
                ""仕訳ID"", ""仕訳日"", ""摘要"", ""ステータス"", ""削除済み"",
                ""作成日時"", ""更新日時"", ""承認者"", ""承認コメント""
            ) VALUES (
                @Id, @EntryDate, @Description, @Status, @Deleted,
                @CreatedAt, @UpdatedAt, @ApprovedBy, @ApprovalComment
            )
            ";

        await connection.ExecuteAsync(sql, new
        {
            Id = id,
            EntryDate = entryDate,
            Description = description,
            Status = status,
            Deleted = deleted,
            CreatedAt = createdAt,
            UpdatedAt = updatedAt,
            ApprovedBy = approvedBy,
            ApprovalComment = approvalComment
        });
    }

    public async Task InsertJournalEntryLineAsync(
        string journalEntryId,
        string accountCode,
        string debitCredit,
        decimal amount)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            INSERT INTO ""仕訳明細ReadModel"" (
                ""仕訳ID"", ""勘定科目コード"", ""貸借区分"", ""金額""
            ) VALUES (
                @JournalEntryId, @AccountCode, @DebitCredit, @Amount
            )
            ";

        await connection.ExecuteAsync(sql, new
        {
            JournalEntryId = journalEntryId,
            AccountCode = accountCode,
            DebitCredit = debitCredit,
            Amount = amount
        });
    }

    public async Task UpdateJournalEntryStatusAsync(
        string id,
        string status,
        DateTime updatedAt,
        string approvedBy,
        string approvalComment)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            UPDATE ""仕訳ReadModel""
            SET ""ステータス"" = @Status,
                ""更新日時"" = @UpdatedAt,
                ""承認者"" = @ApprovedBy,
                ""承認コメント"" = @ApprovalComment
            WHERE ""仕訳ID"" = @Id
            ";

        await connection.ExecuteAsync(sql, new
        {
            Id = id,
            Status = status,
            UpdatedAt = updatedAt,
            ApprovedBy = approvedBy,
            ApprovalComment = approvalComment
        });
    }

    public async Task MarkAsDeletedAsync(string id, DateTime updatedAt)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            UPDATE ""仕訳ReadModel""
            SET ""削除済み"" = true, ""更新日時"" = @UpdatedAt
            WHERE ""仕訳ID"" = @Id
            ";

        await connection.ExecuteAsync(sql, new { Id = id, UpdatedAt = updatedAt });
    }

    public async Task<JournalEntryReadModel?> SelectByIdAsync(string id)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT ""仕訳ID"" as Id,
                   ""仕訳日"" as EntryDate,
                   ""摘要"" as Description,
                   ""ステータス"" as Status,
                   ""削除済み"" as Deleted,
                   ""作成日時"" as CreatedAt,
                   ""更新日時"" as UpdatedAt,
                   ""承認者"" as ApprovedBy,
                   ""承認コメント"" as ApprovalComment
            FROM ""仕訳ReadModel""
            WHERE ""仕訳ID"" = @Id
            ";

        return await connection.QuerySingleOrDefaultAsync<JournalEntryReadModel>(sql, new { Id = id });
    }

    public async Task<IReadOnlyList<JournalEntryReadModel>> SelectByDateRangeAsync(
        DateOnly startDate,
        DateOnly endDate)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT ""仕訳ID"" as Id,
                   ""仕訳日"" as EntryDate,
                   ""摘要"" as Description,
                   ""ステータス"" as Status,
                   ""削除済み"" as Deleted,
                   ""作成日時"" as CreatedAt,
                   ""更新日時"" as UpdatedAt,
                   ""承認者"" as ApprovedBy,
                   ""承認コメント"" as ApprovalComment
            FROM ""仕訳ReadModel""
            WHERE ""仕訳日"" >= @StartDate
              AND ""仕訳日"" <= @EndDate
              AND ""削除済み"" = false
            ORDER BY ""仕訳日"", ""仕訳ID""
            ";

        var results = await connection.QueryAsync<JournalEntryReadModel>(
            sql, new { StartDate = startDate, EndDate = endDate });
        return results.ToList();
    }

    public async Task<IReadOnlyList<JournalEntryLineReadModel>> SelectLinesByJournalEntryIdAsync(
        string journalEntryId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT ""明細ID"" as Id,
                   ""仕訳ID"" as JournalEntryId,
                   ""勘定科目コード"" as AccountCode,
                   ""貸借区分"" as DebitCredit,
                   ""金額"" as Amount
            FROM ""仕訳明細ReadModel""
            WHERE ""仕訳ID"" = @JournalEntryId
            ORDER BY ""明細ID""
            ";

        var results = await connection.QueryAsync<JournalEntryLineReadModel>(
            sql, new { JournalEntryId = journalEntryId });
        return results.ToList();
    }
}
