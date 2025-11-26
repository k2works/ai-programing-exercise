using AccountingSystem.Infrastructure.Repositories;
using Dapper;
using Npgsql;

namespace AccountingSystem.Application.Services;

/// <summary>
/// 残高管理サービス
/// 仕訳入力時の日次残高即時更新を提供
/// </summary>
public class BalanceService
{
    private readonly string _connectionString;

    static BalanceService()
    {
        // DateOnly の TypeHandler を登録
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public BalanceService(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <summary>
    /// 日次残高を更新（UPSERT）
    /// 同じキーが存在する場合は金額を加算、存在しない場合は新規登録
    /// </summary>
    /// <param name="entryDate">起票日</param>
    /// <param name="accountCode">勘定科目コード</param>
    /// <param name="subAccountCode">補助科目コード</param>
    /// <param name="departmentCode">部門コード</param>
    /// <param name="projectCode">プロジェクトコード</param>
    /// <param name="settlementFlag">決算仕訳フラグ</param>
    /// <param name="debitAmount">借方金額</param>
    /// <param name="creditAmount">貸方金額</param>
    public async Task UpdateDailyBalanceAsync(
        DateOnly entryDate,
        string accountCode,
        string? subAccountCode,
        string? departmentCode,
        string? projectCode,
        int? settlementFlag,
        decimal debitAmount,
        decimal creditAmount)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        // PostgreSQL の ON CONFLICT ... DO UPDATE を使用
        var sql = @"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"",
                ""プロジェクトコード"", ""決算仕訳フラグ"", ""借方金額"", ""貸方金額""
            ) VALUES (@EntryDate, @AccountCode, @SubAccountCode, @DepartmentCode, @ProjectCode, @SettlementFlag, @DebitAmount, @CreditAmount)
            ON CONFLICT (""起票日"", ""勘定科目コード"", ""補助科目コード"", ""部門コード"", ""プロジェクトコード"", ""決算仕訳フラグ"")
            DO UPDATE SET
                ""借方金額"" = ""日次勘定科目残高"".""借方金額"" + EXCLUDED.""借方金額"",
                ""貸方金額"" = ""日次勘定科目残高"".""貸方金額"" + EXCLUDED.""貸方金額"",
                ""更新日時"" = CURRENT_TIMESTAMP
            ";

        await connection.ExecuteAsync(sql, new
        {
            EntryDate = entryDate,
            AccountCode = accountCode,
            SubAccountCode = subAccountCode ?? "",
            DepartmentCode = departmentCode ?? "",
            ProjectCode = projectCode ?? "",
            SettlementFlag = settlementFlag ?? 0,
            DebitAmount = debitAmount,
            CreditAmount = creditAmount
        });
    }

    /// <summary>
    /// 指定した日付・勘定科目の日次残高を取得
    /// </summary>
    public async Task<decimal> GetDailyBalanceAsync(
        DateOnly entryDate,
        string accountCode,
        string? subAccountCode = null,
        string? departmentCode = null,
        string? projectCode = null,
        int? settlementFlag = null)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT COALESCE(SUM(""借方金額"" - ""貸方金額""), 0)
            FROM ""日次勘定科目残高""
            WHERE ""起票日"" = @EntryDate
              AND ""勘定科目コード"" = @AccountCode
              AND (""補助科目コード"" = @SubAccountCode OR @SubAccountCode IS NULL)
              AND (""部門コード"" = @DepartmentCode OR @DepartmentCode IS NULL)
              AND (""プロジェクトコード"" = @ProjectCode OR @ProjectCode IS NULL)
              AND (""決算仕訳フラグ"" = @SettlementFlag OR @SettlementFlag IS NULL)
            ";

        return await connection.ExecuteScalarAsync<decimal>(sql, new
        {
            EntryDate = entryDate,
            AccountCode = accountCode,
            SubAccountCode = subAccountCode,
            DepartmentCode = departmentCode,
            ProjectCode = projectCode,
            SettlementFlag = settlementFlag
        });
    }

    /// <summary>
    /// 指定した期間の日次残高合計を取得
    /// </summary>
    public async Task<(decimal DebitTotal, decimal CreditTotal)> GetDailyBalanceTotalAsync(
        DateOnly fromDate,
        DateOnly toDate,
        string accountCode,
        string? subAccountCode = null,
        string? departmentCode = null,
        string? projectCode = null,
        bool excludeSettlement = true)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT
                COALESCE(SUM(""借方金額""), 0) as DebitTotal,
                COALESCE(SUM(""貸方金額""), 0) as CreditTotal
            FROM ""日次勘定科目残高""
            WHERE ""起票日"" >= @FromDate
              AND ""起票日"" <= @ToDate
              AND ""勘定科目コード"" = @AccountCode
              AND (""補助科目コード"" = @SubAccountCode OR @SubAccountCode IS NULL)
              AND (""部門コード"" = @DepartmentCode OR @DepartmentCode IS NULL)
              AND (""プロジェクトコード"" = @ProjectCode OR @ProjectCode IS NULL)
              AND (""決算仕訳フラグ"" = 0 OR @ExcludeSettlement = false)
            ";

        var result = await connection.QuerySingleAsync<(decimal DebitTotal, decimal CreditTotal)>(sql, new
        {
            FromDate = fromDate,
            ToDate = toDate,
            AccountCode = accountCode,
            SubAccountCode = subAccountCode,
            DepartmentCode = departmentCode,
            ProjectCode = projectCode,
            ExcludeSettlement = excludeSettlement
        });

        return result;
    }
}
