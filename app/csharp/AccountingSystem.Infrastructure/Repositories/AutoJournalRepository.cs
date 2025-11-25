using AccountingSystem.Infrastructure.Entities;
using Dapper;
using Npgsql;

namespace AccountingSystem.Infrastructure.Repositories;

/// <summary>
/// 自動仕訳リポジトリ
/// 自動仕訳管理、パターン、実行ログのCRUD操作を提供
/// </summary>
public class AutoJournalRepository
{
    private readonly string _connectionString;

    public AutoJournalRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    #region 自動仕訳管理

    /// <summary>
    /// 自動仕訳管理を登録
    /// </summary>
    public async Task<long> InsertManagementAsync(AutoJournalManagement management)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleAsync<long>(@"
            INSERT INTO ""自動仕訳管理"" (""ソーステーブル名"", ""最終処理日時"")
            VALUES (@SourceTableName, @LastProcessedAt)
            RETURNING ""自動仕訳管理ID""
            ", management);
    }

    /// <summary>
    /// ソーステーブル名で自動仕訳管理を取得
    /// </summary>
    public async Task<AutoJournalManagement?> FindManagementBySourceTableAsync(string sourceTableName)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<AutoJournalManagement>(@"
            SELECT
                ""自動仕訳管理ID"" AS AutoJournalManagementId,
                ""ソーステーブル名"" AS SourceTableName,
                ""最終処理日時"" AS LastProcessedAt,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""自動仕訳管理""
            WHERE ""ソーステーブル名"" = @SourceTableName
            ", new { SourceTableName = sourceTableName });
    }

    /// <summary>
    /// 最終処理日時を更新
    /// </summary>
    public async Task UpdateLastProcessedAtAsync(long managementId, DateTime lastProcessedAt)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(@"
            UPDATE ""自動仕訳管理""
            SET ""最終処理日時"" = @LastProcessedAt, ""更新日時"" = CURRENT_TIMESTAMP
            WHERE ""自動仕訳管理ID"" = @ManagementId
            ", new { ManagementId = managementId, LastProcessedAt = lastProcessedAt });
    }

    #endregion

    #region 自動仕訳パターン

    /// <summary>
    /// 自動仕訳パターンを登録（明細を含む）
    /// </summary>
    public async Task<long> InsertPatternAsync(AutoJournalPattern pattern)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();
        await using var transaction = await connection.BeginTransactionAsync();

        try
        {
            // 1. パターン登録
            var patternId = await connection.QuerySingleAsync<long>(@"
                INSERT INTO ""自動仕訳パターン"" (
                    ""パターンコード"", ""パターン名"", ""ソーステーブル名"",
                    ""説明"", ""有効フラグ""
                )
                VALUES (@PatternCode, @PatternName, @SourceTableName, @Description, @IsActive)
                RETURNING ""自動仕訳パターンID""
                ", pattern, transaction);

            // 2. 明細登録
            foreach (var detail in pattern.Details)
            {
                await connection.ExecuteAsync(@"
                    INSERT INTO ""自動仕訳パターン明細"" (
                        ""自動仕訳パターンID"", ""行番号"", ""貸借区分"",
                        ""勘定科目コード"", ""金額算出式"", ""摘要テンプレート""
                    )
                    VALUES (@AutoJournalPatternId, @LineNumber, @DebitCreditFlag,
                            @AccountCode, @AmountExpression, @DescriptionTemplate)
                    ", new
                {
                    AutoJournalPatternId = patternId,
                    detail.LineNumber,
                    detail.DebitCreditFlag,
                    detail.AccountCode,
                    detail.AmountExpression,
                    detail.DescriptionTemplate
                }, transaction);
            }

            await transaction.CommitAsync();
            return patternId;
        }
        catch
        {
            await transaction.RollbackAsync();
            throw;
        }
    }

    /// <summary>
    /// パターンIDでパターンを取得（明細を含む）
    /// </summary>
    public async Task<AutoJournalPattern?> FindPatternByIdAsync(long patternId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var pattern = await connection.QueryFirstOrDefaultAsync<AutoJournalPattern>(@"
            SELECT
                ""自動仕訳パターンID"" AS AutoJournalPatternId,
                ""パターンコード"" AS PatternCode,
                ""パターン名"" AS PatternName,
                ""ソーステーブル名"" AS SourceTableName,
                ""説明"" AS Description,
                ""有効フラグ"" AS IsActive,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""自動仕訳パターン""
            WHERE ""自動仕訳パターンID"" = @PatternId
            ", new { PatternId = patternId });

        if (pattern == null) return null;

        var details = (await connection.QueryAsync<AutoJournalPatternDetail>(@"
            SELECT
                ""自動仕訳パターン明細ID"" AS AutoJournalPatternDetailId,
                ""自動仕訳パターンID"" AS AutoJournalPatternId,
                ""行番号"" AS LineNumber,
                ""貸借区分"" AS DebitCreditFlag,
                ""勘定科目コード"" AS AccountCode,
                ""金額算出式"" AS AmountExpression,
                ""摘要テンプレート"" AS DescriptionTemplate,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""自動仕訳パターン明細""
            WHERE ""自動仕訳パターンID"" = @PatternId
            ORDER BY ""行番号""
            ", new { PatternId = patternId })).ToList();

        return pattern with { Details = details };
    }

    /// <summary>
    /// パターンコードでパターンを取得
    /// </summary>
    public async Task<AutoJournalPattern?> FindPatternByCodeAsync(string patternCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var pattern = await connection.QueryFirstOrDefaultAsync<AutoJournalPattern>(@"
            SELECT
                ""自動仕訳パターンID"" AS AutoJournalPatternId,
                ""パターンコード"" AS PatternCode,
                ""パターン名"" AS PatternName,
                ""ソーステーブル名"" AS SourceTableName,
                ""説明"" AS Description,
                ""有効フラグ"" AS IsActive,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""自動仕訳パターン""
            WHERE ""パターンコード"" = @PatternCode
            ", new { PatternCode = patternCode });

        if (pattern == null) return null;

        var details = (await connection.QueryAsync<AutoJournalPatternDetail>(@"
            SELECT
                ""自動仕訳パターン明細ID"" AS AutoJournalPatternDetailId,
                ""自動仕訳パターンID"" AS AutoJournalPatternId,
                ""行番号"" AS LineNumber,
                ""貸借区分"" AS DebitCreditFlag,
                ""勘定科目コード"" AS AccountCode,
                ""金額算出式"" AS AmountExpression,
                ""摘要テンプレート"" AS DescriptionTemplate,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""自動仕訳パターン明細""
            WHERE ""自動仕訳パターンID"" = @PatternId
            ORDER BY ""行番号""
            ", new { PatternId = pattern.AutoJournalPatternId })).ToList();

        return pattern with { Details = details };
    }

    /// <summary>
    /// 有効なパターン一覧を取得
    /// </summary>
    public async Task<IEnumerable<AutoJournalPattern>> FindActivePattersAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryAsync<AutoJournalPattern>(@"
            SELECT
                ""自動仕訳パターンID"" AS AutoJournalPatternId,
                ""パターンコード"" AS PatternCode,
                ""パターン名"" AS PatternName,
                ""ソーステーブル名"" AS SourceTableName,
                ""説明"" AS Description,
                ""有効フラグ"" AS IsActive,
                ""作成日時"" AS CreatedAt,
                ""更新日時"" AS UpdatedAt
            FROM ""自動仕訳パターン""
            WHERE ""有効フラグ"" = true
            ORDER BY ""パターンコード""
            ");
    }

    /// <summary>
    /// パターンを削除（CASCADEで明細とログも削除される）
    /// </summary>
    public async Task DeletePatternAsync(long patternId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(@"
            DELETE FROM ""自動仕訳パターン"" WHERE ""自動仕訳パターンID"" = @PatternId
            ", new { PatternId = patternId });
    }

    #endregion

    #region 自動仕訳実行ログ

    /// <summary>
    /// 実行ログを登録
    /// </summary>
    public async Task<long> InsertExecutionLogAsync(AutoJournalExecutionLog log)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleAsync<long>(@"
            INSERT INTO ""自動仕訳実行ログ"" (
                ""自動仕訳パターンID"", ""実行日時"", ""処理件数"",
                ""生成件数"", ""ステータス"", ""メッセージ"", ""エラー詳細""
            )
            VALUES (@AutoJournalPatternId, @ExecutedAt, @ProcessedCount,
                    @GeneratedCount, @Status, @Message, @ErrorDetail)
            RETURNING ""自動仕訳実行ログID""
            ", log);
    }

    /// <summary>
    /// パターンIDで実行ログ一覧を取得
    /// </summary>
    public async Task<IEnumerable<AutoJournalExecutionLog>> FindExecutionLogsByPatternIdAsync(long patternId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryAsync<AutoJournalExecutionLog>(@"
            SELECT
                ""自動仕訳実行ログID"" AS AutoJournalExecutionLogId,
                ""自動仕訳パターンID"" AS AutoJournalPatternId,
                ""実行日時"" AS ExecutedAt,
                ""処理件数"" AS ProcessedCount,
                ""生成件数"" AS GeneratedCount,
                ""ステータス"" AS Status,
                ""メッセージ"" AS Message,
                ""エラー詳細"" AS ErrorDetail,
                ""作成日時"" AS CreatedAt
            FROM ""自動仕訳実行ログ""
            WHERE ""自動仕訳パターンID"" = @PatternId
            ORDER BY ""実行日時"" DESC
            ", new { PatternId = patternId });
    }

    /// <summary>
    /// 最新の実行ログを取得
    /// </summary>
    public async Task<AutoJournalExecutionLog?> FindLatestExecutionLogAsync(long patternId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QueryFirstOrDefaultAsync<AutoJournalExecutionLog>(@"
            SELECT
                ""自動仕訳実行ログID"" AS AutoJournalExecutionLogId,
                ""自動仕訳パターンID"" AS AutoJournalPatternId,
                ""実行日時"" AS ExecutedAt,
                ""処理件数"" AS ProcessedCount,
                ""生成件数"" AS GeneratedCount,
                ""ステータス"" AS Status,
                ""メッセージ"" AS Message,
                ""エラー詳細"" AS ErrorDetail,
                ""作成日時"" AS CreatedAt
            FROM ""自動仕訳実行ログ""
            WHERE ""自動仕訳パターンID"" = @PatternId
            ORDER BY ""実行日時"" DESC
            LIMIT 1
            ", new { PatternId = patternId });
    }

    #endregion
}
