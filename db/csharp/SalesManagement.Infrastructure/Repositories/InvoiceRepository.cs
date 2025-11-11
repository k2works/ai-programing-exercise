using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories;

/// <summary>
/// 請求データのRepositoryクラス
/// </summary>
public class InvoiceRepository
{
    private readonly string _connectionString;

    public InvoiceRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <summary>
    /// 請求番号で検索
    /// </summary>
    public async Task<Invoice?> FindByIdAsync(string invoiceNo)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                請求番号 AS InvoiceNo,
                請求日 AS InvoiceDate,
                得意先コード AS CustomerCode,
                売上番号 AS SalesNo,
                請求額 AS InvoiceAmount,
                請求消込金額 AS ClearedAmount,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 請求データ
            WHERE 請求番号 = @InvoiceNo";

        return await connection.QuerySingleOrDefaultAsync<Invoice>(
            sql, new { InvoiceNo = invoiceNo });
    }

    /// <summary>
    /// 得意先コードで検索
    /// </summary>
    public async Task<IEnumerable<Invoice>> FindByCustomerAsync(string customerCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                請求番号 AS InvoiceNo,
                請求日 AS InvoiceDate,
                得意先コード AS CustomerCode,
                売上番号 AS SalesNo,
                請求額 AS InvoiceAmount,
                請求消込金額 AS ClearedAmount,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 請求データ
            WHERE 得意先コード = @CustomerCode
            ORDER BY 請求日 DESC, 請求番号 DESC";

        return await connection.QueryAsync<Invoice>(
            sql, new { CustomerCode = customerCode });
    }

    /// <summary>
    /// 未消込の請求を検索
    /// </summary>
    public async Task<IEnumerable<Invoice>> FindUnclearedAsync(string customerCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                請求番号 AS InvoiceNo,
                請求日 AS InvoiceDate,
                得意先コード AS CustomerCode,
                売上番号 AS SalesNo,
                請求額 AS InvoiceAmount,
                請求消込金額 AS ClearedAmount,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 請求データ
            WHERE 得意先コード = @CustomerCode
              AND 請求額 > 請求消込金額
            ORDER BY 請求日, 請求番号";

        return await connection.QueryAsync<Invoice>(
            sql, new { CustomerCode = customerCode });
    }

    /// <summary>
    /// 請求を登録
    /// </summary>
    public async Task InsertAsync(Invoice invoice)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            INSERT INTO 請求データ (
                請求番号, 請求日, 得意先コード, 売上番号,
                請求額, 請求消込金額, 備考, 部門コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @InvoiceNo, @InvoiceDate, @CustomerCode, @SalesNo,
                @InvoiceAmount, @ClearedAmount, @Remarks, @DepartmentCode,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, invoice);
    }

    /// <summary>
    /// 請求を更新
    /// </summary>
    public async Task UpdateAsync(Invoice invoice)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 請求データ SET
                請求日 = @InvoiceDate,
                得意先コード = @CustomerCode,
                売上番号 = @SalesNo,
                請求額 = @InvoiceAmount,
                請求消込金額 = @ClearedAmount,
                備考 = @Remarks,
                部門コード = @DepartmentCode,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 請求番号 = @InvoiceNo";

        await connection.ExecuteAsync(sql, invoice);
    }

    /// <summary>
    /// 請求消込金額を更新
    /// </summary>
    public async Task UpdateClearedAmountAsync(string invoiceNo, decimal clearedAmount)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 請求データ SET
                請求消込金額 = 請求消込金額 + @ClearedAmount,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 請求番号 = @InvoiceNo";

        await connection.ExecuteAsync(
            sql, new { InvoiceNo = invoiceNo, ClearedAmount = clearedAmount });
    }

    /// <summary>
    /// 請求を削除
    /// </summary>
    public async Task DeleteAsync(string invoiceNo)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = "DELETE FROM 請求データ WHERE 請求番号 = @InvoiceNo";

        await connection.ExecuteAsync(sql, new { InvoiceNo = invoiceNo });
    }
}
