using System.Data;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using Dapper;
using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Domain;

/// <summary>
/// 日次勘定科目残高テーブルのテスト
/// </summary>
public class DailyAccountBalanceTest : DatabaseTestBase
{
    static DailyAccountBalanceTest()
    {
        // DateOnly の TypeHandler を登録
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    [Fact(DisplayName = "日次残高レコードを登録できる")]
    public async Task Test_日次残高レコードを登録できる()
    {
        // Given: 2025-01-15 の普通預金の日次残高
        var entryDate = new DateOnly(2025, 1, 15);
        var accountCode = "1020"; // 普通預金

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 事前準備: 勘定科目マスタに普通預金を登録
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, '普通預金', '資産', 'B', '1')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        // When: 日次残高を登録
        var insertedRows = await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES (@EntryDate, @AccountCode, '', '', '', 0, 100000.00, 0.00)",
            new { EntryDate = entryDate, AccountCode = accountCode });

        insertedRows.Should().Be(1);

        // Then: データが正しく登録されている
        var allRows = await connection.QueryAsync<dynamic>(@"SELECT * FROM ""日次勘定科目残高""");
        allRows.Should().HaveCount(1);

        var result = allRows.First();
        ((decimal)result.借方金額).Should().Be(100000.00m);
        ((decimal)result.貸方金額).Should().Be(0.00m);
    }

    [Fact(DisplayName = "複合主キーで一意性が保たれる")]
    public async Task Test_複合主キーで一意性が保たれる()
    {
        // Given: 同じキーで日次残高を登録
        var entryDate = new DateOnly(2025, 1, 15);
        var accountCode = "1021";

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 事前準備
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, 'テスト科目', '資産', 'B', '1')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES (@EntryDate, @AccountCode, '', '', '', 0, 100000.00, 0.00)",
            new { EntryDate = entryDate, AccountCode = accountCode });

        // When & Then: 同じキーで2回目の登録を試みるとエラー
        var action = async () =>
        {
            await connection.ExecuteAsync(@"
                INSERT INTO ""日次勘定科目残高"" (
                    ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                    ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
                ) VALUES (@EntryDate, @AccountCode, '', '', '', 0, 50000.00, 0.00)",
                new { EntryDate = entryDate, AccountCode = accountCode });
        };

        await action.Should().ThrowAsync<PostgresException>()
            .Where(e => e.Message.Contains("duplicate key") ||
                       e.Message.Contains("23505")); // PostgreSQL 一意制約違反
    }

    [Fact(DisplayName = "部門別の残高を管理できる")]
    public async Task Test_部門別の残高を管理できる()
    {
        // Given: 売上高の部門別日次残高
        var entryDate = new DateOnly(2025, 1, 15);
        var accountCode = "4010"; // 売上高

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 事前準備
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, '売上高', '収益', 'P', '4')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        // When: 部門001と部門002の残高を登録
        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES (@EntryDate, @AccountCode, '', '001', '', 0, 0.00, 300000.00)",
            new { EntryDate = entryDate, AccountCode = accountCode });

        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES (@EntryDate, @AccountCode, '', '002', '', 0, 0.00, 200000.00)",
            new { EntryDate = entryDate, AccountCode = accountCode });

        // Then: 部門別に集計できる
        var results = await connection.QueryAsync<(string 部門コード, decimal 売上合計)>(@"
            SELECT ""部門コード"", SUM(""貸方金額"") as 売上合計
            FROM ""日次勘定科目残高""
            WHERE ""勘定科目コード"" = @AccountCode
            GROUP BY ""部門コード""
            ORDER BY ""部門コード""",
            new { AccountCode = accountCode });

        var list = results.ToList();
        list.Should().HaveCount(2);
        list[0].部門コード.Should().Be("001");
        list[0].売上合計.Should().Be(300000.00m);
        list[1].部門コード.Should().Be("002");
        list[1].売上合計.Should().Be(200000.00m);
    }

    [Fact(DisplayName = "補助科目別の残高を管理できる")]
    public async Task Test_補助科目別の残高を管理できる()
    {
        // Given: 売掛金の補助科目（得意先）別残高
        var entryDate = new DateOnly(2025, 1, 15);
        var accountCode = "1130"; // 売掛金

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 事前準備
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, '売掛金', '資産', 'B', '1')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        // When: 得意先A001とA002の残高を登録
        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES (@EntryDate, @AccountCode, 'A001', '', '', 0, 500000.00, 0.00)",
            new { EntryDate = entryDate, AccountCode = accountCode });

        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES (@EntryDate, @AccountCode, 'A002', '', '', 0, 300000.00, 0.00)",
            new { EntryDate = entryDate, AccountCode = accountCode });

        // Then: 補助科目別に集計できる
        var results = await connection.QueryAsync<(string 補助科目コード, decimal 売掛金合計)>(@"
            SELECT ""補助科目コード"", SUM(""借方金額"") as 売掛金合計
            FROM ""日次勘定科目残高""
            WHERE ""勘定科目コード"" = @AccountCode
            GROUP BY ""補助科目コード""
            ORDER BY ""補助科目コード""",
            new { AccountCode = accountCode });

        var list = results.ToList();
        list.Should().HaveCount(2);
        list[0].補助科目コード.Should().Be("A001");
        list[0].売掛金合計.Should().Be(500000.00m);
        list[1].補助科目コード.Should().Be("A002");
        list[1].売掛金合計.Should().Be(300000.00m);
    }

    [Fact(DisplayName = "決算仕訳フラグで通常仕訳と決算仕訳を分けて管理できる")]
    public async Task Test_決算仕訳フラグで通常仕訳と決算仕訳を分けて管理できる()
    {
        // Given: 通常仕訳と決算仕訳の残高
        var entryDate = new DateOnly(2025, 3, 31);
        var accountCode = "5110"; // 仕入

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 事前準備
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, '仕入', '費用', 'P', '5')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        // When: 通常仕訳と決算仕訳の残高を登録
        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES (@EntryDate, @AccountCode, '', '', '', 0, 1000000.00, 0.00)",
            new { EntryDate = entryDate, AccountCode = accountCode });

        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES (@EntryDate, @AccountCode, '', '', '', 1, 50000.00, 0.00)",
            new { EntryDate = entryDate, AccountCode = accountCode });

        // Then: 決算仕訳フラグで区別できる
        var normalTotal = await connection.ExecuteScalarAsync<decimal?>(@"
            SELECT SUM(""借方金額"")
            FROM ""日次勘定科目残高""
            WHERE ""勘定科目コード"" = @AccountCode AND ""決算仕訳フラグ"" = 0",
            new { AccountCode = accountCode });

        var settlementTotal = await connection.ExecuteScalarAsync<decimal?>(@"
            SELECT SUM(""借方金額"")
            FROM ""日次勘定科目残高""
            WHERE ""勘定科目コード"" = @AccountCode AND ""決算仕訳フラグ"" = 1",
            new { AccountCode = accountCode });

        normalTotal.Should().Be(1000000.00m);
        settlementTotal.Should().Be(50000.00m);
    }

    [Fact(DisplayName = "金額は非負である必要がある")]
    public async Task Test_金額は非負である必要がある()
    {
        // Given: 負の金額
        var entryDate = new DateOnly(2025, 1, 15);
        var accountCode = "1022";

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 事前準備
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, 'テスト科目2', '資産', 'B', '1')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        // When & Then: 負の借方金額を登録するとエラー
        var action = async () =>
        {
            await connection.ExecuteAsync(@"
                INSERT INTO ""日次勘定科目残高"" (
                    ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                    ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
                ) VALUES (@EntryDate, @AccountCode, '', '', '', 0, -100.00, 0.00)",
                new { EntryDate = entryDate, AccountCode = accountCode });
        };

        await action.Should().ThrowAsync<PostgresException>()
            .Where(e => e.Message.Contains("check") || e.Message.Contains("23514"));
    }
}
