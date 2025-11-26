using System.Data;
using AccountingSystem.Infrastructure.Repositories;
using Dapper;
using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Domain;

/// <summary>
/// 月次勘定科目残高テーブルのテスト
/// </summary>
public class MonthlyAccountBalanceTest : DatabaseTestBase
{
    static MonthlyAccountBalanceTest()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    [Fact(DisplayName = "月次残高レコードを登録できる")]
    public async Task Test_月次残高レコードを登録できる()
    {
        // Given: 2025年度1月の普通預金の月次残高
        var fiscalYear = 20250000;
        var month = 1;
        var accountCode = "1020";

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 事前準備: 勘定科目マスタに普通預金を登録
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, '普通預金', '資産', 'B', '1')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        // When: 月次残高を登録
        await connection.ExecuteAsync(@"
            INSERT INTO ""月次勘定科目残高"" (
                ""決算期"", ""月度"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""月初残高"", ""借方金額"", ""貸方金額"", ""月末残高""
            ) VALUES (@FiscalYear, @Month, @AccountCode, '', '', '', 0, 100000.00, 50000.00, 30000.00, 120000.00)",
            new { FiscalYear = fiscalYear, Month = month, AccountCode = accountCode });

        // Then: データが正しく登録されている
        var allRows = await connection.QueryAsync<dynamic>(@"SELECT * FROM ""月次勘定科目残高""");
        allRows.Should().HaveCount(1);

        var result = allRows.First();
        ((int)result.決算期).Should().Be(20250000);
        ((int)result.月度).Should().Be(1);
        ((decimal)result.月初残高).Should().Be(100000.00m);
        ((decimal)result.借方金額).Should().Be(50000.00m);
        ((decimal)result.貸方金額).Should().Be(30000.00m);
        ((decimal)result.月末残高).Should().Be(120000.00m);
    }

    [Fact(DisplayName = "複合主キーで一意性が保たれる")]
    public async Task Test_複合主キーで一意性が保たれる()
    {
        // Given: 同じキーで月次残高を登録
        var fiscalYear = 20250000;
        var month = 1;
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
            INSERT INTO ""月次勘定科目残高"" (
                ""決算期"", ""月度"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""月初残高"", ""借方金額"", ""貸方金額"", ""月末残高""
            ) VALUES (@FiscalYear, @Month, @AccountCode, '', '', '', 0, 100000.00, 50000.00, 30000.00, 120000.00)",
            new { FiscalYear = fiscalYear, Month = month, AccountCode = accountCode });

        // When & Then: 同じキーで2回目の登録を試みるとエラー
        var action = async () =>
        {
            await connection.ExecuteAsync(@"
                INSERT INTO ""月次勘定科目残高"" (
                    ""決算期"", ""月度"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                    ""プロジェクトコード"", ""決算仕訳フラグ"", ""月初残高"", ""借方金額"", ""貸方金額"", ""月末残高""
                ) VALUES (@FiscalYear, @Month, @AccountCode, '', '', '', 0, 200000.00, 10000.00, 5000.00, 205000.00)",
                new { FiscalYear = fiscalYear, Month = month, AccountCode = accountCode });
        };

        await action.Should().ThrowAsync<PostgresException>()
            .Where(e => e.Message.Contains("duplicate key") ||
                       e.Message.Contains("23505"));
    }

    [Fact(DisplayName = "月度は1から12の範囲である必要がある")]
    public async Task Test_月度は1から12の範囲である必要がある()
    {
        // Given: 無効な月度
        var fiscalYear = 20250000;
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

        // When & Then: 月度13を登録するとエラー
        var action = async () =>
        {
            await connection.ExecuteAsync(@"
                INSERT INTO ""月次勘定科目残高"" (
                    ""決算期"", ""月度"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                    ""プロジェクトコード"", ""決算仕訳フラグ"", ""月初残高"", ""借方金額"", ""貸方金額"", ""月末残高""
                ) VALUES (@FiscalYear, 13, @AccountCode, '', '', '', 0, 0.00, 0.00, 0.00, 0.00)",
                new { FiscalYear = fiscalYear, AccountCode = accountCode });
        };

        await action.Should().ThrowAsync<PostgresException>()
            .Where(e => e.Message.Contains("check") || e.Message.Contains("23514"));
    }

    [Fact(DisplayName = "年間の月次残高を集計できる")]
    public async Task Test_年間の月次残高を集計できる()
    {
        // Given: 複数月の残高データ
        var fiscalYear = 20250000;
        var accountCode = "4010";

        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 事前準備
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, '売上高', '収益', 'P', '4')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        // When: 1月～3月の残高を登録
        for (int month = 1; month <= 3; month++)
        {
            await connection.ExecuteAsync(@"
                INSERT INTO ""月次勘定科目残高"" (
                    ""決算期"", ""月度"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                    ""プロジェクトコード"", ""決算仕訳フラグ"", ""月初残高"", ""借方金額"", ""貸方金額"", ""月末残高""
                ) VALUES (@FiscalYear, @Month, @AccountCode, '', '', '', 0, 0.00, 0.00, @Credit, @Credit)",
                new { FiscalYear = fiscalYear, Month = month, AccountCode = accountCode, Credit = month * 100000m });
        }

        // Then: 年間合計を集計できる
        var totalCredit = await connection.ExecuteScalarAsync<decimal?>(@"
            SELECT SUM(""貸方金額"")
            FROM ""月次勘定科目残高""
            WHERE ""決算期"" = @FiscalYear AND ""勘定科目コード"" = @AccountCode",
            new { FiscalYear = fiscalYear, AccountCode = accountCode });

        totalCredit.Should().Be(600000.00m); // 100000 + 200000 + 300000
    }
}
