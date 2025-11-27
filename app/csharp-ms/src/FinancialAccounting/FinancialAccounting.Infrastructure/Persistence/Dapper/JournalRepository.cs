using Dapper;
using FinancialAccounting.Application.Ports.Out;
using FinancialAccounting.Domain.Entities;
using Microsoft.Extensions.Configuration;
using Npgsql;

namespace FinancialAccounting.Infrastructure.Persistence.Dapper;

/// <summary>
/// 仕訳リポジトリ実装（Dapper）
/// </summary>
public class JournalRepository : IJournalRepository
{
    private readonly string _connectionString;

    public JournalRepository(IConfiguration configuration)
    {
        _connectionString = configuration.GetConnectionString("FinancialAccounting")!;
    }

    // Dapper 用の内部 DTO（PostgreSQL date 型の変換問題を回避）
    private record JournalDto(
        int JournalId,
        DateOnly JournalDate,
        string Description,
        int FiscalYear,
        DateTime CreatedAt,
        DateTime UpdatedAt);

    private static Journal MapToJournal(JournalDto dto)
    {
        return new Journal
        {
            JournalId = dto.JournalId,
            JournalDate = dto.JournalDate.ToDateTime(TimeOnly.MinValue),
            Description = dto.Description,
            FiscalYear = dto.FiscalYear,
            CreatedAt = dto.CreatedAt,
            UpdatedAt = dto.UpdatedAt
        };
    }

    public async Task<Journal> SaveAsync(Journal journal)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();
        await using var transaction = await connection.BeginTransactionAsync();

        try
        {
            // 仕訳ヘッダーを挿入
            var sql = @"
                INSERT INTO journals (journal_date, description, fiscal_year, created_at, updated_at)
                VALUES (@JournalDate, @Description, @FiscalYear, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
                RETURNING journal_id";

            var journalId = await connection.ExecuteScalarAsync<int>(
                sql,
                new { journal.JournalDate, journal.Description, journal.FiscalYear },
                transaction);

            journal.JournalId = journalId;

            // 仕訳明細を挿入
            foreach (var entry in journal.Entries)
            {
                var entrySql = @"
                    INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
                    VALUES (@JournalId, @AccountCode, @DebitAmount, @CreditAmount, @Description)";

                await connection.ExecuteAsync(
                    entrySql,
                    new
                    {
                        JournalId = journalId,
                        entry.AccountCode,
                        entry.DebitAmount,
                        entry.CreditAmount,
                        entry.Description
                    },
                    transaction);
            }

            await transaction.CommitAsync();
            return journal;
        }
        catch
        {
            await transaction.RollbackAsync();
            throw;
        }
    }

    public async Task<Journal?> FindByIdAsync(int journalId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = @"
            SELECT journal_id AS JournalId, journal_date AS JournalDate,
                   description AS Description, fiscal_year AS FiscalYear,
                   created_at AS CreatedAt, updated_at AS UpdatedAt
            FROM journals
            WHERE journal_id = @JournalId";

        var dto = await connection.QuerySingleOrDefaultAsync<JournalDto>(sql, new { JournalId = journalId });
        return dto != null ? MapToJournal(dto) : null;
    }

    public async Task<List<Journal>> FindByFiscalYearAsync(int fiscalYear)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = @"
            SELECT journal_id AS JournalId, journal_date AS JournalDate,
                   description AS Description, fiscal_year AS FiscalYear,
                   created_at AS CreatedAt, updated_at AS UpdatedAt
            FROM journals
            WHERE fiscal_year = @FiscalYear
            ORDER BY journal_date";

        var dtos = await connection.QueryAsync<JournalDto>(sql, new { FiscalYear = fiscalYear });
        return dtos.Select(MapToJournal).ToList();
    }

    public async Task<List<JournalEntry>> FindEntriesByJournalIdAsync(int journalId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = @"
            SELECT entry_id AS EntryId, journal_id AS JournalId,
                   account_code AS AccountCode, debit_amount AS DebitAmount,
                   credit_amount AS CreditAmount, description AS Description
            FROM journal_entries
            WHERE journal_id = @JournalId
            ORDER BY entry_id";

        var entries = await connection.QueryAsync<JournalEntry>(sql, new { JournalId = journalId });
        return entries.ToList();
    }
}
