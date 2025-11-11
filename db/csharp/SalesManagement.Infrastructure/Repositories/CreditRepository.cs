using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories;

/// <summary>
/// 入金データのRepositoryクラス
/// </summary>
public class CreditRepository
{
    private readonly string _connectionString;

    public CreditRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <summary>
    /// 入金伝票番号で検索
    /// </summary>
    public async Task<Credit?> FindByIdAsync(string creditSlipNo)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                入金伝票番号 AS CreditSlipNo,
                入金日 AS CreditDate,
                得意先コード AS CustomerCode,
                入金額 AS CreditAmount,
                入金消込金額 AS ClearedAmount,
                口座コード AS AccountCode,
                入金方法 AS CreditMethod,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                社員コード AS EmployeeCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 入金データ
            WHERE 入金伝票番号 = @CreditSlipNo";

        return await connection.QuerySingleOrDefaultAsync<Credit>(
            sql, new { CreditSlipNo = creditSlipNo });
    }

    /// <summary>
    /// 得意先コードで検索
    /// </summary>
    public async Task<IEnumerable<Credit>> FindByCustomerAsync(string customerCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                入金伝票番号 AS CreditSlipNo,
                入金日 AS CreditDate,
                得意先コード AS CustomerCode,
                入金額 AS CreditAmount,
                入金消込金額 AS ClearedAmount,
                口座コード AS AccountCode,
                入金方法 AS CreditMethod,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                社員コード AS EmployeeCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 入金データ
            WHERE 得意先コード = @CustomerCode
            ORDER BY 入金日 DESC, 入金伝票番号 DESC";

        return await connection.QueryAsync<Credit>(
            sql, new { CustomerCode = customerCode });
    }

    /// <summary>
    /// 未消込の入金を検索
    /// </summary>
    public async Task<IEnumerable<Credit>> FindUnclearedAsync(string customerCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                入金伝票番号 AS CreditSlipNo,
                入金日 AS CreditDate,
                得意先コード AS CustomerCode,
                入金額 AS CreditAmount,
                入金消込金額 AS ClearedAmount,
                口座コード AS AccountCode,
                入金方法 AS CreditMethod,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                社員コード AS EmployeeCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 入金データ
            WHERE 得意先コード = @CustomerCode
              AND 入金額 > 入金消込金額
            ORDER BY 入金日, 入金伝票番号";

        return await connection.QueryAsync<Credit>(
            sql, new { CustomerCode = customerCode });
    }

    /// <summary>
    /// 入金を登録
    /// </summary>
    public async Task InsertAsync(Credit credit)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            INSERT INTO 入金データ (
                入金伝票番号, 入金日, 得意先コード, 入金額, 入金消込金額,
                口座コード, 入金方法, 備考, 部門コード, 社員コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @CreditSlipNo, @CreditDate, @CustomerCode, @CreditAmount, @ClearedAmount,
                @AccountCode, @CreditMethod, @Remarks, @DepartmentCode, @EmployeeCode,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, credit);
    }

    /// <summary>
    /// 入金を更新
    /// </summary>
    public async Task UpdateAsync(Credit credit)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 入金データ SET
                入金日 = @CreditDate,
                得意先コード = @CustomerCode,
                入金額 = @CreditAmount,
                入金消込金額 = @ClearedAmount,
                口座コード = @AccountCode,
                入金方法 = @CreditMethod,
                備考 = @Remarks,
                部門コード = @DepartmentCode,
                社員コード = @EmployeeCode,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 入金伝票番号 = @CreditSlipNo";

        await connection.ExecuteAsync(sql, credit);
    }

    /// <summary>
    /// 入金消込金額を更新
    /// </summary>
    public async Task UpdateClearedAmountAsync(string creditSlipNo, decimal clearedAmount)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 入金データ SET
                入金消込金額 = 入金消込金額 + @ClearedAmount,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 入金伝票番号 = @CreditSlipNo";

        await connection.ExecuteAsync(
            sql, new { CreditSlipNo = creditSlipNo, ClearedAmount = clearedAmount });
    }

    /// <summary>
    /// 入金を削除
    /// </summary>
    public async Task DeleteAsync(string creditSlipNo)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = "DELETE FROM 入金データ WHERE 入金伝票番号 = @CreditSlipNo";

        await connection.ExecuteAsync(sql, new { CreditSlipNo = creditSlipNo });
    }
}
