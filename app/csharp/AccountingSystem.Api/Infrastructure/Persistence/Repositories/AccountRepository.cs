using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Infrastructure.Persistence.Dapper.Entities;
using Dapper;
using Npgsql;

namespace AccountingSystem.Infrastructure.Persistence.Repositories;

/// <summary>
/// 勘定科目リポジトリ
/// </summary>
public class AccountRepository : IAccountRepository
{
    private readonly string _connectionString;

    // 共通のSELECT句（ENUM型を文字列にキャスト）
    private const string SelectColumns = @"
        ""勘定科目ID"",
        ""勘定科目コード"",
        ""勘定科目名"",
        ""勘定科目カナ"",
        ""勘定科目種別""::text AS ""勘定科目種別"",
        ""合計科目"",
        ""BSPL区分"",
        ""取引要素区分"",
        ""費用区分"",
        ""表示順序"",
        ""集計対象"",
        ""課税取引コード"",
        ""残高"",
        ""作成日時"",
        ""更新日時""
    ";

    static AccountRepository()
    {
        // Dapper のカスタムプロパティマッピング設定
        SqlMapper.SetTypeMap(
            typeof(Account),
            new CustomPropertyTypeMap(
                typeof(Account),
                (type, columnName) => columnName switch
                {
                    "勘定科目ID" => type.GetProperty(nameof(Account.AccountId)),
                    "勘定科目コード" => type.GetProperty(nameof(Account.AccountCode)),
                    "勘定科目名" => type.GetProperty(nameof(Account.AccountName)),
                    "勘定科目カナ" => type.GetProperty(nameof(Account.AccountNameKana)),
                    "勘定科目種別" => type.GetProperty(nameof(Account.AccountType)),
                    "合計科目" => type.GetProperty(nameof(Account.IsSummaryAccount)),
                    "BSPL区分" => type.GetProperty(nameof(Account.BsplType)),
                    "取引要素区分" => type.GetProperty(nameof(Account.TransactionElementType)),
                    "費用区分" => type.GetProperty(nameof(Account.ExpenseType)),
                    "表示順序" => type.GetProperty(nameof(Account.DisplayOrder)),
                    "集計対象" => type.GetProperty(nameof(Account.IsAggregationTarget)),
                    "課税取引コード" => type.GetProperty(nameof(Account.TaxCode)),
                    "残高" => type.GetProperty(nameof(Account.Balance)),
                    "作成日時" => type.GetProperty(nameof(Account.CreatedAt)),
                    "更新日時" => type.GetProperty(nameof(Account.UpdatedAt)),
                    _ => null
                }
            )
        );
    }

    public AccountRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task<int> InsertAsync(Account account)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = @"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"",
                ""勘定科目名"",
                ""勘定科目カナ"",
                ""勘定科目種別"",
                ""合計科目"",
                ""BSPL区分"",
                ""取引要素区分"",
                ""費用区分"",
                ""表示順序"",
                ""集計対象"",
                ""課税取引コード"",
                ""残高"",
                ""作成日時"",
                ""更新日時""
            ) VALUES (
                @AccountCode,
                @AccountName,
                @AccountNameKana,
                @AccountType::account_type,
                @IsSummaryAccount,
                @BsplType,
                @TransactionElementType,
                @ExpenseType,
                @DisplayOrder,
                @IsAggregationTarget,
                @TaxCode,
                @Balance,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
            RETURNING ""勘定科目ID""
        ";

        return await connection.ExecuteScalarAsync<int>(sql, account);
    }

    public async Task<Account?> FindByCodeAsync(string accountCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = $@"
            SELECT {SelectColumns} FROM ""勘定科目マスタ""
            WHERE ""勘定科目コード"" = @AccountCode
        ";

        return await connection.QuerySingleOrDefaultAsync<Account>(sql, new { AccountCode = accountCode });
    }

    public async Task<Account?> FindByIdAsync(int accountId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = $@"
            SELECT {SelectColumns} FROM ""勘定科目マスタ""
            WHERE ""勘定科目ID"" = @AccountId
        ";

        return await connection.QuerySingleOrDefaultAsync<Account>(sql, new { AccountId = accountId });
    }

    public async Task<IEnumerable<Account>> FindAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = $@"
            SELECT {SelectColumns} FROM ""勘定科目マスタ""
            ORDER BY ""勘定科目コード""
        ";

        return await connection.QueryAsync<Account>(sql);
    }

    public async Task<IEnumerable<Account>> FindByTypeAsync(string accountType)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = $@"
            SELECT {SelectColumns} FROM ""勘定科目マスタ""
            WHERE ""勘定科目種別"" = @AccountType::account_type
            ORDER BY ""表示順序"", ""勘定科目コード""
        ";

        return await connection.QueryAsync<Account>(sql, new { AccountType = accountType });
    }

    public async Task<IEnumerable<Account>> FindSummaryAccountsAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = $@"
            SELECT {SelectColumns} FROM ""勘定科目マスタ""
            WHERE ""合計科目"" = true
            ORDER BY ""表示順序"", ""勘定科目コード""
        ";

        return await connection.QueryAsync<Account>(sql);
    }

    public async Task<IEnumerable<Account>> FindDetailAccountsAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = $@"
            SELECT {SelectColumns} FROM ""勘定科目マスタ""
            WHERE ""合計科目"" = false
            ORDER BY ""表示順序"", ""勘定科目コード""
        ";

        return await connection.QueryAsync<Account>(sql);
    }

    public async Task<int> UpdateAsync(Account account)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = @"
            UPDATE ""勘定科目マスタ""
            SET ""勘定科目名"" = @AccountName,
                ""勘定科目カナ"" = @AccountNameKana,
                ""勘定科目種別"" = @AccountType::account_type,
                ""合計科目"" = @IsSummaryAccount,
                ""BSPL区分"" = @BsplType,
                ""取引要素区分"" = @TransactionElementType,
                ""費用区分"" = @ExpenseType,
                ""表示順序"" = @DisplayOrder,
                ""集計対象"" = @IsAggregationTarget,
                ""課税取引コード"" = @TaxCode,
                ""残高"" = @Balance,
                ""更新日時"" = CURRENT_TIMESTAMP
            WHERE ""勘定科目コード"" = @AccountCode
        ";

        return await connection.ExecuteAsync(sql, account);
    }

    public async Task<int> UpdateBalanceAsync(string accountCode, decimal balance)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = @"
            UPDATE ""勘定科目マスタ""
            SET ""残高"" = @Balance,
                ""更新日時"" = CURRENT_TIMESTAMP
            WHERE ""勘定科目コード"" = @AccountCode
        ";

        return await connection.ExecuteAsync(sql, new { AccountCode = accountCode, Balance = balance });
    }

    public async Task<int> DeleteAsync(string accountCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);

        var sql = @"
            DELETE FROM ""勘定科目マスタ""
            WHERE ""勘定科目コード"" = @AccountCode
        ";

        return await connection.ExecuteAsync(sql, new { AccountCode = accountCode });
    }
}
