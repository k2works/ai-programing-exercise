using System.Data;
using AccountingSystem.Infrastructure.Repositories;
using Dapper;
using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Balance;

/// <summary>
/// 総勘定元帳・試算表ビューのテスト
/// </summary>
public class LedgerViewsTest : DatabaseTestBase
{
    static LedgerViewsTest()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    [Fact(DisplayName = "総勘定元帳ビューから累積残高を取得できる")]
    public async Task Test_総勘定元帳ビューから累積残高を取得できる()
    {
        // Given: 勘定科目と複数日の日次残高
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        var accountCode = "1020";
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, '普通預金', '資産', 'B', '1')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        // When: 3日分の日次残高を登録
        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES
                (@Date1, @AccountCode, '', '', '', 0, 100000.00, 0.00),
                (@Date2, @AccountCode, '', '', '', 0, 50000.00, 20000.00),
                (@Date3, @AccountCode, '', '', '', 0, 30000.00, 10000.00)",
            new
            {
                Date1 = new DateOnly(2025, 1, 10),
                Date2 = new DateOnly(2025, 1, 15),
                Date3 = new DateOnly(2025, 1, 20),
                AccountCode = accountCode
            });

        // Then: 総勘定元帳から累積残高を取得できる
        var ledger = await connection.QueryAsync<dynamic>(@"
            SELECT * FROM ""総勘定元帳""
            WHERE account_code = @AccountCode
            ORDER BY entry_date",
            new { AccountCode = accountCode });

        var list = ledger.ToList();
        list.Should().HaveCount(3);

        // 1日目: 100000 - 0 = 100000
        ((decimal)list[0].balance).Should().Be(100000.00m);

        // 2日目: 100000 + 50000 - 20000 = 130000
        ((decimal)list[1].balance).Should().Be(130000.00m);

        // 3日目: 130000 + 30000 - 10000 = 150000
        ((decimal)list[2].balance).Should().Be(150000.00m);
    }

    [Fact(DisplayName = "試算表ビューから勘定科目別残高を取得できる")]
    public async Task Test_試算表ビューから勘定科目別残高を取得できる()
    {
        // Given: 複数の勘定科目と日次残高
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        // 資産科目（普通預金）
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES ('1020', '普通預金', '資産', 'B', '1')
            ON CONFLICT (""勘定科目コード"") DO NOTHING");

        // 負債科目（買掛金）
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES ('2010', '買掛金', '負債', 'B', '2')
            ON CONFLICT (""勘定科目コード"") DO NOTHING");

        // 収益科目（売上高）
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES ('4010', '売上高', '収益', 'P', '4')
            ON CONFLICT (""勘定科目コード"") DO NOTHING");

        // When: 日次残高を登録
        var entryDate = new DateOnly(2025, 1, 15);
        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES
                (@EntryDate, '1020', '', '', '', 0, 500000.00, 100000.00),
                (@EntryDate, '2010', '', '', '', 0, 50000.00, 200000.00),
                (@EntryDate, '4010', '', '', '', 0, 0.00, 300000.00)",
            new { EntryDate = entryDate });

        // Then: 試算表から残高を取得できる
        var trialBalance = await connection.QueryAsync<dynamic>(@"
            SELECT * FROM ""試算表""
            WHERE account_code IN ('1020', '2010', '4010')
            ORDER BY account_code");

        var list = trialBalance.ToList();
        list.Should().HaveCount(3);

        // 普通預金（資産）: 借方500000 - 貸方100000 = 400000
        var asset = list.First(x => (string)x.account_code == "1020");
        ((decimal)asset.debit_total).Should().Be(500000.00m);
        ((decimal)asset.credit_total).Should().Be(100000.00m);
        ((decimal)asset.balance).Should().Be(400000.00m);

        // 買掛金（負債）: 貸方200000 - 借方50000 = 150000
        var liability = list.First(x => (string)x.account_code == "2010");
        ((decimal)liability.debit_total).Should().Be(50000.00m);
        ((decimal)liability.credit_total).Should().Be(200000.00m);
        ((decimal)liability.balance).Should().Be(150000.00m);

        // 売上高（収益）: 貸方300000 - 借方0 = 300000
        var revenue = list.First(x => (string)x.account_code == "4010");
        ((decimal)revenue.debit_total).Should().Be(0.00m);
        ((decimal)revenue.credit_total).Should().Be(300000.00m);
        ((decimal)revenue.balance).Should().Be(300000.00m);
    }

    [Fact(DisplayName = "決算仕訳は総勘定元帳・試算表から除外される")]
    public async Task Test_決算仕訳は総勘定元帳試算表から除外される()
    {
        // Given: 勘定科目を登録
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        var accountCode = "5110";
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, '仕入', '費用', 'P', '5')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        var entryDate = new DateOnly(2025, 3, 31);

        // When: 通常仕訳と決算仕訳を登録
        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES
                (@EntryDate, @AccountCode, '', '', '', 0, 1000000.00, 0.00),
                (@EntryDate, @AccountCode, '', '', '', 1, 50000.00, 0.00)",
            new { EntryDate = entryDate, AccountCode = accountCode });

        // Then: 総勘定元帳には通常仕訳のみ
        var ledger = await connection.QueryAsync<dynamic>(@"
            SELECT * FROM ""総勘定元帳""
            WHERE account_code = @AccountCode",
            new { AccountCode = accountCode });

        ledger.Should().HaveCount(1);
        ((decimal)ledger.First().debit_amount).Should().Be(1000000.00m);

        // 試算表にも通常仕訳のみ
        var trialBalance = await connection.QuerySingleOrDefaultAsync<dynamic>(@"
            SELECT * FROM ""試算表""
            WHERE account_code = @AccountCode",
            new { AccountCode = accountCode });

        ((decimal)trialBalance!.debit_total).Should().Be(1000000.00m);
        ((decimal)trialBalance.balance).Should().Be(1000000.00m);
    }
}
