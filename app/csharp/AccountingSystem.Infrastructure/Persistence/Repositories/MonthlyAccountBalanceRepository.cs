using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Entities;
using Dapper;
using Npgsql;

namespace AccountingSystem.Infrastructure.Persistence.Repositories;

/// <summary>
/// 月次勘定科目残高リポジトリ
/// </summary>
public class MonthlyAccountBalanceRepository : IMonthlyAccountBalanceRepository
{
    private readonly string _connectionString;

    // 共通のSELECT句
    private const string SelectColumns = @"
        ""決算期"",
        ""月度"",
        ""勘定科目コード"",
        ""補助科目コード"",
        ""部門コード"",
        ""プロジェクトコード"",
        ""決算仕訳フラグ"",
        ""月初残高"",
        ""借方金額"",
        ""貸方金額"",
        ""月末残高"",
        ""作成日時"",
        ""更新日時""
    ";

    static MonthlyAccountBalanceRepository()
    {
        // Dapper のカスタムプロパティマッピング設定
        SqlMapper.SetTypeMap(
            typeof(MonthlyAccountBalance),
            new CustomPropertyTypeMap(
                typeof(MonthlyAccountBalance),
                (type, columnName) => columnName switch
                {
                    "決算期" => type.GetProperty(nameof(MonthlyAccountBalance.FiscalYear)),
                    "月度" => type.GetProperty(nameof(MonthlyAccountBalance.Month)),
                    "勘定科目コード" => type.GetProperty(nameof(MonthlyAccountBalance.AccountCode)),
                    "補助科目コード" => type.GetProperty(nameof(MonthlyAccountBalance.SubAccountCode)),
                    "部門コード" => type.GetProperty(nameof(MonthlyAccountBalance.DepartmentCode)),
                    "プロジェクトコード" => type.GetProperty(nameof(MonthlyAccountBalance.ProjectCode)),
                    "決算仕訳フラグ" => type.GetProperty(nameof(MonthlyAccountBalance.SettlementFlag)),
                    "月初残高" => type.GetProperty(nameof(MonthlyAccountBalance.OpeningBalance)),
                    "借方金額" => type.GetProperty(nameof(MonthlyAccountBalance.DebitAmount)),
                    "貸方金額" => type.GetProperty(nameof(MonthlyAccountBalance.CreditAmount)),
                    "月末残高" => type.GetProperty(nameof(MonthlyAccountBalance.ClosingBalance)),
                    "作成日時" => type.GetProperty(nameof(MonthlyAccountBalance.CreatedAt)),
                    "更新日時" => type.GetProperty(nameof(MonthlyAccountBalance.UpdatedAt)),
                    _ => null
                }
            )
        );
    }

    public MonthlyAccountBalanceRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <inheritdoc />
    public async Task<IReadOnlyList<MonthlyAccountBalance>> FindByFiscalYearAndMonthAsync(int fiscalYear, int month)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = $@"
            SELECT {SelectColumns}
            FROM ""月次勘定科目残高""
            WHERE ""決算期"" = @FiscalYear
              AND ""月度"" = @Month
            ORDER BY ""勘定科目コード""
        ";

        var result = await connection.QueryAsync<MonthlyAccountBalance>(
            sql,
            new { FiscalYear = fiscalYear, Month = month });

        return result.ToList();
    }

    /// <inheritdoc />
    public async Task<IReadOnlyList<MonthlyAccountBalance>> FindByFiscalYearAsync(int fiscalYear)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = $@"
            SELECT {SelectColumns}
            FROM ""月次勘定科目残高""
            WHERE ""決算期"" = @FiscalYear
            ORDER BY ""月度"", ""勘定科目コード""
        ";

        var result = await connection.QueryAsync<MonthlyAccountBalance>(
            sql,
            new { FiscalYear = fiscalYear });

        return result.ToList();
    }

    /// <inheritdoc />
    public async Task<IReadOnlyList<MonthlyAccountBalance>> FindByAccountCodeAsync(int fiscalYear, string accountCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = $@"
            SELECT {SelectColumns}
            FROM ""月次勘定科目残高""
            WHERE ""決算期"" = @FiscalYear
              AND ""勘定科目コード"" = @AccountCode
            ORDER BY ""月度""
        ";

        var result = await connection.QueryAsync<MonthlyAccountBalance>(
            sql,
            new { FiscalYear = fiscalYear, AccountCode = accountCode });

        return result.ToList();
    }
}
