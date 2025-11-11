using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories;

/// <summary>
/// 請求データ明細のRepositoryクラス
/// </summary>
public class InvoiceDetailRepository
{
    private readonly string _connectionString;

    public InvoiceDetailRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <summary>
    /// 請求番号で明細を検索
    /// </summary>
    public async Task<IEnumerable<InvoiceDetail>> FindByInvoiceNoAsync(string invoiceNo)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                請求番号 AS InvoiceNo,
                請求明細番号 AS InvoiceDetailNo,
                売上番号 AS SalesNo,
                売上行番号 AS SalesLineNo,
                請求額 AS InvoiceAmount,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 請求データ明細
            WHERE 請求番号 = @InvoiceNo
            ORDER BY 請求明細番号";

        return await connection.QueryAsync<InvoiceDetail>(
            sql, new { InvoiceNo = invoiceNo });
    }

    /// <summary>
    /// 請求明細を登録
    /// </summary>
    public async Task InsertAsync(InvoiceDetail invoiceDetail)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            INSERT INTO 請求データ明細 (
                請求番号, 請求明細番号, 売上番号, 売上行番号, 請求額,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @InvoiceNo, @InvoiceDetailNo, @SalesNo, @SalesLineNo, @InvoiceAmount,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, invoiceDetail);
    }

    /// <summary>
    /// 請求番号で明細を削除
    /// </summary>
    public async Task DeleteByInvoiceNoAsync(string invoiceNo)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = "DELETE FROM 請求データ明細 WHERE 請求番号 = @InvoiceNo";

        await connection.ExecuteAsync(sql, new { InvoiceNo = invoiceNo });
    }
}
