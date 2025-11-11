using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories;

/// <summary>
/// 入金口座マスタのRepositoryクラス
/// </summary>
public class BankAccountRepository
{
    private readonly string _connectionString;

    public BankAccountRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <summary>
    /// 口座コードで検索
    /// </summary>
    public async Task<BankAccount?> FindByIdAsync(string accountCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                口座コード AS AccountCode,
                口座名 AS AccountName,
                銀行名 AS BankName,
                支店名 AS BranchName,
                口座番号 AS AccountNumber,
                口座種別 AS AccountType,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 入金口座マスタ
            WHERE 口座コード = @AccountCode";

        return await connection.QuerySingleOrDefaultAsync<BankAccount>(
            sql, new { AccountCode = accountCode });
    }

    /// <summary>
    /// すべての口座を取得
    /// </summary>
    public async Task<IEnumerable<BankAccount>> FindAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                口座コード AS AccountCode,
                口座名 AS AccountName,
                銀行名 AS BankName,
                支店名 AS BranchName,
                口座番号 AS AccountNumber,
                口座種別 AS AccountType,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 入金口座マスタ
            ORDER BY 口座コード";

        return await connection.QueryAsync<BankAccount>(sql);
    }

    /// <summary>
    /// 口座を登録
    /// </summary>
    public async Task InsertAsync(BankAccount bankAccount)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            INSERT INTO 入金口座マスタ (
                口座コード, 口座名, 銀行名, 支店名, 口座番号, 口座種別,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @AccountCode, @AccountName, @BankName, @BranchName,
                @AccountNumber, @AccountType,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, bankAccount);
    }

    /// <summary>
    /// 口座を更新
    /// </summary>
    public async Task UpdateAsync(BankAccount bankAccount)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 入金口座マスタ SET
                口座名 = @AccountName,
                銀行名 = @BankName,
                支店名 = @BranchName,
                口座番号 = @AccountNumber,
                口座種別 = @AccountType,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 口座コード = @AccountCode";

        await connection.ExecuteAsync(sql, bankAccount);
    }

    /// <summary>
    /// 口座を削除
    /// </summary>
    public async Task DeleteAsync(string accountCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = "DELETE FROM 入金口座マスタ WHERE 口座コード = @AccountCode";

        await connection.ExecuteAsync(sql, new { AccountCode = accountCode });
    }
}
