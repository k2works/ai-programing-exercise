using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Domain;

/// <summary>
/// 自動仕訳テーブルのテスト
///
/// 自動仕訳システムは、売上データや給与データなど他システムから
/// 定期的に連携されるデータを自動的に仕訳に変換します。
/// 日付管理方式を採用し、元データを変更せずに差分処理を実現します。
/// </summary>
public class AutoJournalTest : DatabaseTestBase
{
    private async Task CleanupAsync()
    {
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 各テスト前にデータをクリア（外部キー制約を考慮した順序）
        await using var command1 = new NpgsqlCommand(@"TRUNCATE TABLE ""自動仕訳実行ログ"" CASCADE", connection);
        await command1.ExecuteNonQueryAsync();

        await using var command2 = new NpgsqlCommand(@"TRUNCATE TABLE ""自動仕訳パターン明細"" CASCADE", connection);
        await command2.ExecuteNonQueryAsync();

        await using var command3 = new NpgsqlCommand(@"TRUNCATE TABLE ""自動仕訳パターン"" CASCADE", connection);
        await command3.ExecuteNonQueryAsync();

        await using var command4 = new NpgsqlCommand(@"TRUNCATE TABLE ""自動仕訳管理"" CASCADE", connection);
        await command4.ExecuteNonQueryAsync();

        await using var command5 = new NpgsqlCommand(@"TRUNCATE TABLE ""勘定科目マスタ"" CASCADE", connection);
        await command5.ExecuteNonQueryAsync();

        // テスト用勘定科目を登録
        await InsertTestAccountsAsync(connection);
    }

    [Fact(DisplayName = "自動仕訳管理テーブル_最終処理日時を管理できる")]
    public async Task Test_AutoJournalManagement_LastProcessedAt()
    {
        await CleanupAsync();

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 1. 自動仕訳管理レコードを登録
        var insertSql = @"
            INSERT INTO ""自動仕訳管理"" (""ソーステーブル名"", ""最終処理日時"")
            VALUES (@SourceTableName, @LastProcessedAt)
            RETURNING ""自動仕訳管理ID""
        ";

        long managementId;
        var initialProcessedAt = new DateTime(2024, 1, 15, 10, 0, 0);

        await using (var command = new NpgsqlCommand(insertSql, connection))
        {
            command.Parameters.AddWithValue("SourceTableName", "売上データ");
            command.Parameters.AddWithValue("LastProcessedAt", initialProcessedAt);
            managementId = (long)(await command.ExecuteScalarAsync())!;
        }

        managementId.Should().BeGreaterThan(0);

        // 2. 最終処理日時を更新
        var newProcessedAt = new DateTime(2024, 1, 16, 15, 30, 0);
        var updateSql = @"
            UPDATE ""自動仕訳管理""
            SET ""最終処理日時"" = @LastProcessedAt, ""更新日時"" = CURRENT_TIMESTAMP
            WHERE ""自動仕訳管理ID"" = @Id
        ";

        await using (var command = new NpgsqlCommand(updateSql, connection))
        {
            command.Parameters.AddWithValue("LastProcessedAt", newProcessedAt);
            command.Parameters.AddWithValue("Id", managementId);
            var updated = await command.ExecuteNonQueryAsync();
            updated.Should().Be(1);
        }

        // 3. 更新後の値を検証
        var selectSql = @"
            SELECT ""最終処理日時""
            FROM ""自動仕訳管理""
            WHERE ""自動仕訳管理ID"" = @Id
        ";

        await using (var command = new NpgsqlCommand(selectSql, connection))
        {
            command.Parameters.AddWithValue("Id", managementId);
            var result = await command.ExecuteScalarAsync();
            var lastProcessedAt = (DateTime)result!;
            lastProcessedAt.Should().Be(newProcessedAt);
        }
    }

    [Fact(DisplayName = "自動仕訳パターン_仕訳生成ルールを定義できる")]
    public async Task Test_AutoJournalPattern_CanDefineRule()
    {
        await CleanupAsync();

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 1. 自動仕訳パターンを登録
        var patternSql = @"
            INSERT INTO ""自動仕訳パターン"" (
                ""パターンコード"", ""パターン名"", ""ソーステーブル名"",
                ""説明"", ""有効フラグ""
            )
            VALUES (@PatternCode, @PatternName, @SourceTableName, @Description, @IsActive)
            RETURNING ""自動仕訳パターンID""
        ";

        long patternId;
        await using (var command = new NpgsqlCommand(patternSql, connection))
        {
            command.Parameters.AddWithValue("PatternCode", "SALES_001");
            command.Parameters.AddWithValue("PatternName", "売上仕訳パターン");
            command.Parameters.AddWithValue("SourceTableName", "売上データ");
            command.Parameters.AddWithValue("Description", "売上データから仕訳を自動生成するパターン");
            command.Parameters.AddWithValue("IsActive", true);
            patternId = (long)(await command.ExecuteScalarAsync())!;
        }

        patternId.Should().BeGreaterThan(0);

        // 2. パターン明細を登録（借方：売掛金）
        var itemSql = @"
            INSERT INTO ""自動仕訳パターン明細"" (
                ""自動仕訳パターンID"", ""行番号"", ""貸借区分"",
                ""勘定科目コード"", ""金額算出式"", ""摘要テンプレート""
            )
            VALUES (@PatternId, @LineNumber, @DebitCreditFlag, @AccountCode, @AmountExpression, @DescriptionTemplate)
        ";

        // 借方：売掛金
        await using (var command = new NpgsqlCommand(itemSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            command.Parameters.AddWithValue("LineNumber", 1);
            command.Parameters.AddWithValue("DebitCreditFlag", "D");
            command.Parameters.AddWithValue("AccountCode", "1300"); // 売掛金
            command.Parameters.AddWithValue("AmountExpression", "税込金額");
            command.Parameters.AddWithValue("DescriptionTemplate", "売上: {顧客名}");
            await command.ExecuteNonQueryAsync();
        }

        // 貸方：売上
        await using (var command = new NpgsqlCommand(itemSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            command.Parameters.AddWithValue("LineNumber", 2);
            command.Parameters.AddWithValue("DebitCreditFlag", "C");
            command.Parameters.AddWithValue("AccountCode", "4100"); // 売上
            command.Parameters.AddWithValue("AmountExpression", "税抜金額");
            command.Parameters.AddWithValue("DescriptionTemplate", "売上: {顧客名}");
            await command.ExecuteNonQueryAsync();
        }

        // 貸方：仮受消費税
        await using (var command = new NpgsqlCommand(itemSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            command.Parameters.AddWithValue("LineNumber", 3);
            command.Parameters.AddWithValue("DebitCreditFlag", "C");
            command.Parameters.AddWithValue("AccountCode", "2120"); // 仮受消費税
            command.Parameters.AddWithValue("AmountExpression", "消費税額");
            command.Parameters.AddWithValue("DescriptionTemplate", "消費税: {顧客名}");
            await command.ExecuteNonQueryAsync();
        }

        // 3. パターンと明細を検証
        var countSql = @"
            SELECT COUNT(*)
            FROM ""自動仕訳パターン明細""
            WHERE ""自動仕訳パターンID"" = @PatternId
        ";

        await using (var command = new NpgsqlCommand(countSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            var count = (long)(await command.ExecuteScalarAsync())!;
            count.Should().Be(3);
        }
    }

    [Fact(DisplayName = "自動仕訳実行ログ_処理状況を記録できる")]
    public async Task Test_AutoJournalLog_CanRecordExecution()
    {
        await CleanupAsync();

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 1. 自動仕訳パターンを登録
        var patternSql = @"
            INSERT INTO ""自動仕訳パターン"" (
                ""パターンコード"", ""パターン名"", ""ソーステーブル名"", ""有効フラグ""
            )
            VALUES ('LOG_TEST', 'ログテスト用パターン', 'テストデータ', true)
            RETURNING ""自動仕訳パターンID""
        ";

        long patternId;
        await using (var command = new NpgsqlCommand(patternSql, connection))
        {
            patternId = (long)(await command.ExecuteScalarAsync())!;
        }

        // 2. 実行ログを登録（成功）
        var logSql = @"
            INSERT INTO ""自動仕訳実行ログ"" (
                ""自動仕訳パターンID"", ""実行日時"", ""処理件数"",
                ""生成件数"", ""ステータス"", ""メッセージ""
            )
            VALUES (@PatternId, @ExecutedAt, @ProcessedCount, @GeneratedCount, @Status, @Message)
            RETURNING ""自動仕訳実行ログID""
        ";

        await using (var command = new NpgsqlCommand(logSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            command.Parameters.AddWithValue("ExecutedAt", DateTime.Now);
            command.Parameters.AddWithValue("ProcessedCount", 100);
            command.Parameters.AddWithValue("GeneratedCount", 100);
            command.Parameters.AddWithValue("Status", "SUCCESS");
            command.Parameters.AddWithValue("Message", "正常終了");
            var logId = (long)(await command.ExecuteScalarAsync())!;
            logId.Should().BeGreaterThan(0);
        }

        // 3. 実行ログを登録（エラー）
        await using (var command = new NpgsqlCommand(logSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            command.Parameters.AddWithValue("ExecutedAt", DateTime.Now);
            command.Parameters.AddWithValue("ProcessedCount", 50);
            command.Parameters.AddWithValue("GeneratedCount", 45);
            command.Parameters.AddWithValue("Status", "ERROR");
            command.Parameters.AddWithValue("Message", "一部データの処理に失敗");
            await command.ExecuteNonQueryAsync();
        }

        // 4. ログ件数を検証
        var countSql = @"
            SELECT COUNT(*)
            FROM ""自動仕訳実行ログ""
            WHERE ""自動仕訳パターンID"" = @PatternId
        ";

        await using (var command = new NpgsqlCommand(countSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            var count = (long)(await command.ExecuteScalarAsync())!;
            count.Should().Be(2);
        }
    }

    [Fact(DisplayName = "外部キー制約_パターン削除時に明細とログも削除される")]
    public async Task Test_ForeignKeyConstraint_CascadeDelete()
    {
        await CleanupAsync();

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 1. パターンを登録
        var patternSql = @"
            INSERT INTO ""自動仕訳パターン"" (
                ""パターンコード"", ""パターン名"", ""ソーステーブル名"", ""有効フラグ""
            )
            VALUES ('CASCADE_TEST', 'カスケードテスト', 'テストデータ', true)
            RETURNING ""自動仕訳パターンID""
        ";

        long patternId;
        await using (var command = new NpgsqlCommand(patternSql, connection))
        {
            patternId = (long)(await command.ExecuteScalarAsync())!;
        }

        // 2. 明細を登録
        var itemSql = @"
            INSERT INTO ""自動仕訳パターン明細"" (
                ""自動仕訳パターンID"", ""行番号"", ""貸借区分"",
                ""勘定科目コード"", ""金額算出式""
            )
            VALUES (@PatternId, 1, 'D', '1100', '金額')
        ";

        await using (var command = new NpgsqlCommand(itemSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            await command.ExecuteNonQueryAsync();
        }

        // 3. ログを登録
        var logSql = @"
            INSERT INTO ""自動仕訳実行ログ"" (
                ""自動仕訳パターンID"", ""実行日時"", ""処理件数"",
                ""生成件数"", ""ステータス""
            )
            VALUES (@PatternId, CURRENT_TIMESTAMP, 10, 10, 'SUCCESS')
        ";

        await using (var command = new NpgsqlCommand(logSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            await command.ExecuteNonQueryAsync();
        }

        // 4. パターンを削除
        var deleteSql = @"DELETE FROM ""自動仕訳パターン"" WHERE ""自動仕訳パターンID"" = @PatternId";
        await using (var command = new NpgsqlCommand(deleteSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            var deleted = await command.ExecuteNonQueryAsync();
            deleted.Should().Be(1);
        }

        // 5. 明細とログが削除されたことを検証
        var checkItemSql = @"SELECT COUNT(*) FROM ""自動仕訳パターン明細"" WHERE ""自動仕訳パターンID"" = @PatternId";
        await using (var command = new NpgsqlCommand(checkItemSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            var count = (long)(await command.ExecuteScalarAsync())!;
            count.Should().Be(0);
        }

        var checkLogSql = @"SELECT COUNT(*) FROM ""自動仕訳実行ログ"" WHERE ""自動仕訳パターンID"" = @PatternId";
        await using (var command = new NpgsqlCommand(checkLogSql, connection))
        {
            command.Parameters.AddWithValue("PatternId", patternId);
            var count = (long)(await command.ExecuteScalarAsync())!;
            count.Should().Be(0);
        }
    }

    /// <summary>
    /// テスト用勘定科目を登録するヘルパーメソッド
    /// </summary>
    private static async Task InsertTestAccountsAsync(NpgsqlConnection connection)
    {
        var sql = @"
            INSERT INTO ""勘定科目マスタ"" (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""合計科目"", ""集計対象"", ""残高"")
            VALUES
            ('1100', '現金', '資産', false, true, 0),
            ('1300', '売掛金', '資産', false, true, 0),
            ('2120', '仮受消費税', '負債', false, true, 0),
            ('4100', '売上', '収益', false, true, 0)
            ON CONFLICT DO NOTHING
        ";

        await using var command = new NpgsqlCommand(sql, connection);
        await command.ExecuteNonQueryAsync();
    }
}
