using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests;

/// <summary>
/// 勘定科目マスタの制約テスト
/// </summary>
public class AccountConstraintTest : DatabaseTestBase
{
    [Fact(DisplayName = "BSPL区分は 'B' または 'P' のみ許可")]
    public async Task TestBsplDistinctionConstraint()
    {
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // BSPL区分に不正な値を入れる
        // check_bspl_distinction: BSPL区分 IN ('B', 'P') OR BSPL区分 IS NULL
        var sql = @"
            INSERT INTO ""勘定科目マスタ""
            (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""残高"")
            VALUES (@code, @name, @type::account_type, @bspl, @balance)
        ";

        await using var cmd = new NpgsqlCommand(sql, connection);
        cmd.Parameters.AddWithValue("code", "1000");
        cmd.Parameters.AddWithValue("name", "現金");
        cmd.Parameters.AddWithValue("type", "資産");
        cmd.Parameters.AddWithValue("bspl", "X");  // 不正な値（B, P 以外）
        cmd.Parameters.AddWithValue("balance", 0m);

        var act = async () => await cmd.ExecuteNonQueryAsync();

        // check_bspl_distinction または check_bspl_consistency のいずれかが発動
        await act.Should().ThrowAsync<PostgresException>()
            .Where(ex => ex.ConstraintName != null && ex.ConstraintName.StartsWith("check_bspl"));
    }

    [Fact(DisplayName = "資産科目のBSPL区分は 'B' である必要がある")]
    public async Task TestBsplConsistencyConstraint()
    {
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        var sql = @"
            INSERT INTO ""勘定科目マスタ""
            (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""残高"")
            VALUES (@code, @name, @type::account_type, @bspl, @balance)
        ";

        await using var cmd = new NpgsqlCommand(sql, connection);
        cmd.Parameters.AddWithValue("code", "1001");
        cmd.Parameters.AddWithValue("name", "現金");
        cmd.Parameters.AddWithValue("type", "資産");
        cmd.Parameters.AddWithValue("bspl", "P");  // 不整合
        cmd.Parameters.AddWithValue("balance", 0m);

        var act = async () => await cmd.ExecuteNonQueryAsync();

        await act.Should().ThrowAsync<PostgresException>()
            .Where(ex => ex.Message.Contains("check_bspl_consistency"));
    }

    [Fact(DisplayName = "取引要素区分は '1'〜'5' のみ許可")]
    public async Task TestTransactionDistinctionConstraint()
    {
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 取引要素区分に不正な値を入れる
        // check_transaction_distinction: 取引要素区分 IN ('1', '2', '3', '4', '5') OR 取引要素区分 IS NULL
        var sql = @"
            INSERT INTO ""勘定科目マスタ""
            (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""取引要素区分"", ""残高"")
            VALUES (@code, @name, @type::account_type, @transaction, @balance)
        ";

        await using var cmd = new NpgsqlCommand(sql, connection);
        cmd.Parameters.AddWithValue("code", "1002");
        cmd.Parameters.AddWithValue("name", "現金");
        cmd.Parameters.AddWithValue("type", "資産");
        cmd.Parameters.AddWithValue("transaction", "9");  // 不正な値（1〜5以外）
        cmd.Parameters.AddWithValue("balance", 0m);

        var act = async () => await cmd.ExecuteNonQueryAsync();

        // check_transaction_distinction または check_transaction_consistency のいずれかが発動
        await act.Should().ThrowAsync<PostgresException>()
            .Where(ex => ex.ConstraintName != null && ex.ConstraintName.StartsWith("check_transaction"));
    }
}
