using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories;

/// <summary>
/// 与信残高データのRepositoryクラス
/// </summary>
public class CreditBalanceRepository
{
    private readonly string _connectionString;

    public CreditBalanceRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <summary>
    /// 取引先コードで検索
    /// </summary>
    public async Task<CreditBalance?> FindByIdAsync(string companyCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                取引先コード AS CompanyCode,
                受注残高 AS OrderBalance,
                債権残高 AS ReceivableBalance,
                債務残高 AS PayableBalance,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 与信残高データ
            WHERE 取引先コード = @CompanyCode";

        return await connection.QuerySingleOrDefaultAsync<CreditBalance>(
            sql, new { CompanyCode = companyCode });
    }

    /// <summary>
    /// 全件取得
    /// </summary>
    public async Task<IEnumerable<CreditBalance>> FindAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                取引先コード AS CompanyCode,
                受注残高 AS OrderBalance,
                債権残高 AS ReceivableBalance,
                債務残高 AS PayableBalance,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 与信残高データ
            ORDER BY 取引先コード";

        return await connection.QueryAsync<CreditBalance>(sql);
    }

    /// <summary>
    /// 与信残高を登録
    /// </summary>
    public async Task InsertAsync(CreditBalance creditBalance)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            INSERT INTO 与信残高データ (
                取引先コード, 受注残高, 債権残高, 債務残高,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @CompanyCode, @OrderBalance, @ReceivableBalance, @PayableBalance,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, creditBalance);
    }

    /// <summary>
    /// 与信残高を更新
    /// </summary>
    public async Task UpdateAsync(CreditBalance creditBalance)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 与信残高データ SET
                受注残高 = @OrderBalance,
                債権残高 = @ReceivableBalance,
                債務残高 = @PayableBalance,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 取引先コード = @CompanyCode";

        await connection.ExecuteAsync(sql, creditBalance);
    }

    /// <summary>
    /// 受注残高を更新
    /// </summary>
    public async Task UpdateOrderBalanceAsync(string companyCode, decimal amount)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 与信残高データ SET
                受注残高 = 受注残高 + @Amount,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 取引先コード = @CompanyCode";

        await connection.ExecuteAsync(
            sql, new { CompanyCode = companyCode, Amount = amount });
    }

    /// <summary>
    /// 債権残高を更新
    /// </summary>
    public async Task UpdateReceivableBalanceAsync(string companyCode, decimal amount)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 与信残高データ SET
                債権残高 = 債権残高 + @Amount,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 取引先コード = @CompanyCode";

        await connection.ExecuteAsync(
            sql, new { CompanyCode = companyCode, Amount = amount });
    }

    /// <summary>
    /// 債務残高を更新
    /// </summary>
    public async Task UpdatePayableBalanceAsync(string companyCode, decimal amount)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 与信残高データ SET
                債務残高 = 債務残高 + @Amount,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 取引先コード = @CompanyCode";

        await connection.ExecuteAsync(
            sql, new { CompanyCode = companyCode, Amount = amount });
    }

    /// <summary>
    /// 与信残高を削除
    /// </summary>
    public async Task DeleteAsync(string companyCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = "DELETE FROM 与信残高データ WHERE 取引先コード = @CompanyCode";

        await connection.ExecuteAsync(sql, new { CompanyCode = companyCode });
    }
}
