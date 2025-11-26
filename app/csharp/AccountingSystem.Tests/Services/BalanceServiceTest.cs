using System.Data;
using AccountingSystem.Application.Services;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using Dapper;
using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Services;

/// <summary>
/// 残高管理サービスのテスト
/// </summary>
public class BalanceServiceTest : DatabaseTestBase
{
    static BalanceServiceTest()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    [Fact(DisplayName = "日次残高をUPSERTで更新できる - 新規登録")]
    public async Task Test_日次残高をUPSERTで更新できる_新規登録()
    {
        // Given: 勘定科目を事前登録
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        var accountCode = "1020";
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, '普通預金', '資産', 'B', '1')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        var service = new BalanceService(ConnectionString);
        var entryDate = new DateOnly(2025, 1, 15);

        // When: 日次残高を更新（新規登録）
        await service.UpdateDailyBalanceAsync(
            entryDate,
            accountCode,
            subAccountCode: "",
            departmentCode: "",
            projectCode: "",
            settlementFlag: 0,
            debitAmount: 100000.00m,
            creditAmount: 0.00m);

        // Then: 残高が登録されている
        var allRows = await connection.QueryAsync<dynamic>(@"SELECT * FROM ""日次勘定科目残高""");
        allRows.Should().HaveCount(1);

        var balance = allRows.First();
        ((decimal)balance.借方金額).Should().Be(100000.00m);
        ((decimal)balance.貸方金額).Should().Be(0.00m);
    }

    [Fact(DisplayName = "日次残高をUPSERTで更新できる - 既存レコードに加算")]
    public async Task Test_日次残高をUPSERTで更新できる_既存レコードに加算()
    {
        // Given: 勘定科目と既存の日次残高を事前登録
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        var accountCode = "1021";
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, 'テスト科目', '資産', 'B', '1')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        var entryDate = new DateOnly(2025, 1, 15);
        await connection.ExecuteAsync(@"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES (@EntryDate, @AccountCode, '', '', '', 0, 100000.00, 0.00)",
            new { EntryDate = entryDate, AccountCode = accountCode });

        var service = new BalanceService(ConnectionString);

        // When: 同じキーで日次残高を更新（既存に加算）
        await service.UpdateDailyBalanceAsync(
            entryDate,
            accountCode,
            subAccountCode: "",
            departmentCode: "",
            projectCode: "",
            settlementFlag: 0,
            debitAmount: 50000.00m,
            creditAmount: 30000.00m);

        // Then: 残高が加算されている
        var allRows = await connection.QueryAsync<dynamic>(@"SELECT * FROM ""日次勘定科目残高""");
        allRows.Should().HaveCount(1);

        var balance = allRows.First();
        ((decimal)balance.借方金額).Should().Be(150000.00m); // 100000 + 50000
        ((decimal)balance.貸方金額).Should().Be(30000.00m);   // 0 + 30000
    }

    [Fact(DisplayName = "部門別の日次残高を更新できる")]
    public async Task Test_部門別の日次残高を更新できる()
    {
        // Given: 勘定科目を事前登録
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();

        var accountCode = "4010";
        await connection.ExecuteAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分""
            ) VALUES (@Code, '売上高', '収益', 'P', '4')
            ON CONFLICT (""勘定科目コード"") DO NOTHING",
            new { Code = accountCode });

        var service = new BalanceService(ConnectionString);
        var entryDate = new DateOnly(2025, 1, 15);

        // When: 部門別に日次残高を更新
        await service.UpdateDailyBalanceAsync(
            entryDate, accountCode, "", "001", "", 0, 0.00m, 300000.00m);
        await service.UpdateDailyBalanceAsync(
            entryDate, accountCode, "", "002", "", 0, 0.00m, 200000.00m);

        // Then: 部門ごとに別々の残高が登録されている
        var balances = await connection.QueryAsync<dynamic>(@"
            SELECT * FROM ""日次勘定科目残高""
            WHERE ""勘定科目コード"" = @AccountCode
            ORDER BY ""部門コード""",
            new { AccountCode = accountCode });

        var list = balances.ToList();
        list.Should().HaveCount(2);
        ((string)list[0].部門コード).Should().Be("001");
        ((decimal)list[0].貸方金額).Should().Be(300000.00m);
        ((string)list[1].部門コード).Should().Be("002");
        ((decimal)list[1].貸方金額).Should().Be(200000.00m);
    }
}
