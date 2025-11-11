using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories;

/// <summary>
/// 支払データのRepositoryクラス
/// </summary>
public class PaymentRepository
{
    private readonly string _connectionString;

    public PaymentRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <summary>
    /// 支払伝票番号で検索
    /// </summary>
    public async Task<Payment?> FindByIdAsync(string paymentSlipNo)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                支払伝票番号 AS PaymentSlipNo,
                支払日 AS PaymentDate,
                仕入先コード AS SupplierCode,
                支払額 AS PaymentAmount,
                支払方法 AS PaymentMethod,
                支払完了フラグ AS CompletedFlag,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 支払データ
            WHERE 支払伝票番号 = @PaymentSlipNo";

        return await connection.QuerySingleOrDefaultAsync<Payment>(
            sql, new { PaymentSlipNo = paymentSlipNo });
    }

    /// <summary>
    /// 仕入先コードで検索
    /// </summary>
    public async Task<IEnumerable<Payment>> FindBySupplierAsync(string supplierCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                支払伝票番号 AS PaymentSlipNo,
                支払日 AS PaymentDate,
                仕入先コード AS SupplierCode,
                支払額 AS PaymentAmount,
                支払方法 AS PaymentMethod,
                支払完了フラグ AS CompletedFlag,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 支払データ
            WHERE 仕入先コード = @SupplierCode
            ORDER BY 支払日 DESC, 支払伝票番号 DESC";

        return await connection.QueryAsync<Payment>(
            sql, new { SupplierCode = supplierCode });
    }

    /// <summary>
    /// 未完了の支払を検索
    /// </summary>
    public async Task<IEnumerable<Payment>> FindUncompletedAsync(string supplierCode)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                支払伝票番号 AS PaymentSlipNo,
                支払日 AS PaymentDate,
                仕入先コード AS SupplierCode,
                支払額 AS PaymentAmount,
                支払方法 AS PaymentMethod,
                支払完了フラグ AS CompletedFlag,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 支払データ
            WHERE 仕入先コード = @SupplierCode
              AND 支払完了フラグ = 0
            ORDER BY 支払日, 支払伝票番号";

        return await connection.QueryAsync<Payment>(
            sql, new { SupplierCode = supplierCode });
    }

    /// <summary>
    /// 支払を登録
    /// </summary>
    public async Task InsertAsync(Payment payment)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            INSERT INTO 支払データ (
                支払伝票番号, 支払日, 仕入先コード, 支払額, 支払方法,
                支払完了フラグ, 備考, 部門コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @PaymentSlipNo, @PaymentDate, @SupplierCode, @PaymentAmount, @PaymentMethod,
                @CompletedFlag, @Remarks, @DepartmentCode,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, payment);
    }

    /// <summary>
    /// 支払を更新
    /// </summary>
    public async Task UpdateAsync(Payment payment)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 支払データ SET
                支払日 = @PaymentDate,
                仕入先コード = @SupplierCode,
                支払額 = @PaymentAmount,
                支払方法 = @PaymentMethod,
                支払完了フラグ = @CompletedFlag,
                備考 = @Remarks,
                部門コード = @DepartmentCode,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 支払伝票番号 = @PaymentSlipNo";

        await connection.ExecuteAsync(sql, payment);
    }

    /// <summary>
    /// 支払完了フラグを更新
    /// </summary>
    public async Task UpdateCompletedFlagAsync(string paymentSlipNo, int completedFlag)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 支払データ SET
                支払完了フラグ = @CompletedFlag,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 支払伝票番号 = @PaymentSlipNo";

        await connection.ExecuteAsync(
            sql, new { PaymentSlipNo = paymentSlipNo, CompletedFlag = completedFlag });
    }

    /// <summary>
    /// 支払を削除
    /// </summary>
    public async Task DeleteAsync(string paymentSlipNo)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = "DELETE FROM 支払データ WHERE 支払伝票番号 = @PaymentSlipNo";

        await connection.ExecuteAsync(sql, new { PaymentSlipNo = paymentSlipNo });
    }
}
