using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Domain;

/// <summary>
/// 複式簿記の原理をデータベースで保証するテスト
///
/// PostgreSQL のビューと関数を使って、仕訳の借方・貸方の合計が
/// 一致することをデータベースレベルで検証します。
/// </summary>
public class DoubleEntryCheckTest : DatabaseTestBase
{
    private async Task CleanupAsync()
    {
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 各テスト前にデータをクリア
        await using var command1 = new NpgsqlCommand(@"TRUNCATE TABLE ""仕訳"" CASCADE", connection);
        await command1.ExecuteNonQueryAsync();

        await using var command2 = new NpgsqlCommand(@"TRUNCATE TABLE ""勘定科目マスタ"" CASCADE", connection);
        await command2.ExecuteNonQueryAsync();

        // テスト用勘定科目を登録
        await InsertTestAccountsAsync(connection);
    }

    [Fact(DisplayName = "仕訳残高チェックビュー_借方貸方の合計を算出できる")]
    public async Task Test_BalanceCheckView_CalculatesTotals()
    {
        await CleanupAsync();

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 1. 正しい仕訳を登録（借方 = 貸方）
        await InsertValidJournalEntryAsync(connection, "JE001", 100000m);

        // 2. ビューから残高をチェック
        var sql = @"
            SELECT ""仕訳伝票番号"", ""借方合計"", ""貸方合計"", ""差額""
            FROM ""仕訳残高チェック""
            WHERE ""仕訳伝票番号"" = @VoucherNo
        ";

        await using var command = new NpgsqlCommand(sql, connection);
        command.Parameters.AddWithValue("VoucherNo", "JE001");

        await using var reader = await command.ExecuteReaderAsync();
        reader.Read().Should().BeTrue();

        var debitTotal = reader.GetDecimal(1);
        var creditTotal = reader.GetDecimal(2);
        var difference = reader.GetDecimal(3);

        debitTotal.Should().Be(100000m);
        creditTotal.Should().Be(100000m);
        difference.Should().Be(0m);
    }

    [Fact(DisplayName = "複式簿記チェック関数_整合性のある仕訳は検出されない")]
    public async Task Test_DoubleEntryCheck_ValidEntryNotDetected()
    {
        await CleanupAsync();

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 1. 正しい仕訳を登録
        await InsertValidJournalEntryAsync(connection, "JE001", 100000m);
        await InsertValidJournalEntryAsync(connection, "JE002", 50000m);

        // 2. 複式簿記チェック関数を実行
        var sql = @"SELECT * FROM ""複式簿記チェック""()";

        await using var command = new NpgsqlCommand(sql, connection);
        await using var reader = await command.ExecuteReaderAsync();

        // 不整合な仕訳はないはず
        reader.HasRows.Should().BeFalse();
    }

    [Fact(DisplayName = "複式簿記チェック関数_不整合な仕訳を検出できる")]
    public async Task Test_DoubleEntryCheck_InvalidEntryDetected()
    {
        await CleanupAsync();

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 1. 正しい仕訳を登録
        await InsertValidJournalEntryAsync(connection, "JE001", 100000m);

        // 2. 不整合な仕訳を登録（借方 > 貸方）
        await InsertInvalidJournalEntryAsync(connection, "JE002", 50000m, 45000m);

        // 3. 複式簿記チェック関数を実行
        var sql = @"SELECT ""不整合伝票番号"", ""差額"" FROM ""複式簿記チェック""()";

        await using var command = new NpgsqlCommand(sql, connection);
        await using var reader = await command.ExecuteReaderAsync();

        // 不整合な仕訳が検出されるはず
        reader.HasRows.Should().BeTrue();
        reader.Read().Should().BeTrue();

        var voucherNo = reader.GetString(0);
        var difference = reader.GetDecimal(1);

        voucherNo.Should().Be("JE002");
        difference.Should().Be(5000m); // 50000 - 45000 = 5000
    }

    [Fact(DisplayName = "複数の不整合仕訳を検出できる")]
    public async Task Test_DoubleEntryCheck_DetectsMultipleInvalidEntries()
    {
        await CleanupAsync();

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 1. 正しい仕訳
        await InsertValidJournalEntryAsync(connection, "JE001", 100000m);

        // 2. 不整合な仕訳1（借方 > 貸方）
        await InsertInvalidJournalEntryAsync(connection, "JE002", 50000m, 48000m);

        // 3. 不整合な仕訳2（借方 < 貸方）
        await InsertInvalidJournalEntryAsync(connection, "JE003", 30000m, 35000m);

        // 4. 複式簿記チェック関数を実行
        var sql = @"SELECT COUNT(*) FROM ""複式簿記チェック""()";

        await using var command = new NpgsqlCommand(sql, connection);
        var count = (long)(await command.ExecuteScalarAsync())!;

        count.Should().Be(2); // JE002 と JE003 が検出される
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
            ('4100', '売上', '収益', false, true, 0)
            ON CONFLICT DO NOTHING
        ";

        await using var command = new NpgsqlCommand(sql, connection);
        await command.ExecuteNonQueryAsync();
    }

    /// <summary>
    /// 整合性のある仕訳（借方 = 貸方）を登録するヘルパーメソッド
    /// </summary>
    private static async Task InsertValidJournalEntryAsync(NpgsqlConnection connection, string voucherNo, decimal amount)
    {
        // 仕訳ヘッダー
        var headerSql = @"
            INSERT INTO ""仕訳"" (""仕訳伝票番号"", ""起票日"", ""入力日"")
            VALUES (@VoucherNo, @Date, @Date)
        ";

        await using (var command = new NpgsqlCommand(headerSql, connection))
        {
            command.Parameters.AddWithValue("VoucherNo", voucherNo);
            command.Parameters.AddWithValue("Date", DateTime.Today);
            await command.ExecuteNonQueryAsync();
        }

        // 仕訳明細
        var lineSql = @"
            INSERT INTO ""仕訳明細"" (""仕訳伝票番号"", ""仕訳行番号"", ""行摘要"")
            VALUES (@VoucherNo, 1, '売上取引')
        ";

        await using (var command = new NpgsqlCommand(lineSql, connection))
        {
            command.Parameters.AddWithValue("VoucherNo", voucherNo);
            await command.ExecuteNonQueryAsync();
        }

        // 仕訳貸借明細（借方）
        var debitSql = @"
            INSERT INTO ""仕訳貸借明細"" (
                ""仕訳伝票番号"", ""仕訳行番号"", ""仕訳行貸借区分"",
                ""勘定科目コード"", ""仕訳金額"", ""基軸換算仕訳金額""
            )
            VALUES (@VoucherNo, 1, 'D', '1100', @Amount, @Amount)
        ";

        await using (var command = new NpgsqlCommand(debitSql, connection))
        {
            command.Parameters.AddWithValue("VoucherNo", voucherNo);
            command.Parameters.AddWithValue("Amount", amount);
            await command.ExecuteNonQueryAsync();
        }

        // 仕訳貸借明細（貸方）
        var creditSql = @"
            INSERT INTO ""仕訳貸借明細"" (
                ""仕訳伝票番号"", ""仕訳行番号"", ""仕訳行貸借区分"",
                ""勘定科目コード"", ""仕訳金額"", ""基軸換算仕訳金額""
            )
            VALUES (@VoucherNo, 1, 'C', '4100', @Amount, @Amount)
        ";

        await using (var command = new NpgsqlCommand(creditSql, connection))
        {
            command.Parameters.AddWithValue("VoucherNo", voucherNo);
            command.Parameters.AddWithValue("Amount", amount);
            await command.ExecuteNonQueryAsync();
        }
    }

    /// <summary>
    /// 不整合な仕訳（借方 ≠ 貸方）を登録するヘルパーメソッド
    /// </summary>
    private static async Task InsertInvalidJournalEntryAsync(NpgsqlConnection connection, string voucherNo, decimal debitAmount, decimal creditAmount)
    {
        // 仕訳ヘッダー
        var headerSql = @"
            INSERT INTO ""仕訳"" (""仕訳伝票番号"", ""起票日"", ""入力日"")
            VALUES (@VoucherNo, @Date, @Date)
        ";

        await using (var command = new NpgsqlCommand(headerSql, connection))
        {
            command.Parameters.AddWithValue("VoucherNo", voucherNo);
            command.Parameters.AddWithValue("Date", DateTime.Today);
            await command.ExecuteNonQueryAsync();
        }

        // 仕訳明細
        var lineSql = @"
            INSERT INTO ""仕訳明細"" (""仕訳伝票番号"", ""仕訳行番号"", ""行摘要"")
            VALUES (@VoucherNo, 1, '不整合テスト')
        ";

        await using (var command = new NpgsqlCommand(lineSql, connection))
        {
            command.Parameters.AddWithValue("VoucherNo", voucherNo);
            await command.ExecuteNonQueryAsync();
        }

        // 仕訳貸借明細（借方）
        var debitSql = @"
            INSERT INTO ""仕訳貸借明細"" (
                ""仕訳伝票番号"", ""仕訳行番号"", ""仕訳行貸借区分"",
                ""勘定科目コード"", ""仕訳金額"", ""基軸換算仕訳金額""
            )
            VALUES (@VoucherNo, 1, 'D', '1100', @Amount, @Amount)
        ";

        await using (var command = new NpgsqlCommand(debitSql, connection))
        {
            command.Parameters.AddWithValue("VoucherNo", voucherNo);
            command.Parameters.AddWithValue("Amount", debitAmount);
            await command.ExecuteNonQueryAsync();
        }

        // 仕訳貸借明細（貸方）
        var creditSql = @"
            INSERT INTO ""仕訳貸借明細"" (
                ""仕訳伝票番号"", ""仕訳行番号"", ""仕訳行貸借区分"",
                ""勘定科目コード"", ""仕訳金額"", ""基軸換算仕訳金額""
            )
            VALUES (@VoucherNo, 1, 'C', '4100', @Amount, @Amount)
        ";

        await using (var command = new NpgsqlCommand(creditSql, connection))
        {
            command.Parameters.AddWithValue("VoucherNo", voucherNo);
            command.Parameters.AddWithValue("Amount", creditAmount);
            await command.ExecuteNonQueryAsync();
        }
    }
}
