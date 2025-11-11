using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories;

/// <summary>
/// 自動採番マスタのRepositoryクラス
/// </summary>
public class AutoNumberRepository
{
    private readonly string _connectionString;

    public AutoNumberRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <summary>
    /// 伝票種別と年月で検索
    /// </summary>
    public async Task<AutoNumber?> FindByIdAsync(string slipType, string yearMonth)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                伝票種別コード AS SlipType,
                年月 AS YearMonth,
                最終伝票番号 AS LastSlipNo,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 自動採番マスタ
            WHERE 伝票種別コード = @SlipType
              AND 年月 = @YearMonth";

        return await connection.QuerySingleOrDefaultAsync<AutoNumber>(
            sql, new { SlipType = slipType, YearMonth = yearMonth });
    }

    /// <summary>
    /// 伝票種別と年月で検索（FOR UPDATE）
    /// </summary>
    public async Task<AutoNumber?> FindByIdForUpdateAsync(string slipType, string yearMonth, NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        var sql = @"
            SELECT
                伝票種別コード AS SlipType,
                年月 AS YearMonth,
                最終伝票番号 AS LastSlipNo,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 自動採番マスタ
            WHERE 伝票種別コード = @SlipType
              AND 年月 = @YearMonth
            FOR UPDATE";

        return await connection.QuerySingleOrDefaultAsync<AutoNumber>(
            sql, new { SlipType = slipType, YearMonth = yearMonth }, transaction);
    }

    /// <summary>
    /// 全件取得
    /// </summary>
    public async Task<IEnumerable<AutoNumber>> FindAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            SELECT
                伝票種別コード AS SlipType,
                年月 AS YearMonth,
                最終伝票番号 AS LastSlipNo,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 自動採番マスタ
            ORDER BY 伝票種別コード, 年月";

        return await connection.QueryAsync<AutoNumber>(sql);
    }

    /// <summary>
    /// 自動採番を登録
    /// </summary>
    public async Task InsertAsync(AutoNumber autoNumber)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            INSERT INTO 自動採番マスタ (
                伝票種別コード, 年月, 最終伝票番号,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @SlipType, @YearMonth, @LastSlipNo,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, autoNumber);
    }

    /// <summary>
    /// 自動採番を更新
    /// </summary>
    public async Task UpdateAsync(AutoNumber autoNumber)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 自動採番マスタ SET
                最終伝票番号 = @LastSlipNo,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 伝票種別コード = @SlipType
              AND 年月 = @YearMonth";

        await connection.ExecuteAsync(sql, autoNumber);
    }

    /// <summary>
    /// 最終伝票番号をインクリメント
    /// </summary>
    public async Task IncrementLastSlipNoAsync(string slipType, string yearMonth)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            UPDATE 自動採番マスタ SET
                最終伝票番号 = 最終伝票番号 + 1,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 伝票種別コード = @SlipType
              AND 年月 = @YearMonth";

        await connection.ExecuteAsync(
            sql, new { SlipType = slipType, YearMonth = yearMonth });
    }

    /// <summary>
    /// 伝票番号を生成
    /// </summary>
    public async Task<string> GenerateSlipNoAsync(string slipType)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        await using var transaction = await connection.BeginTransactionAsync();

        try
        {
            var yearMonth = DateTime.Now.ToString("yyyyMM");

            // FOR UPDATEで排他ロック
            var autoNumber = await FindByIdForUpdateAsync(slipType, yearMonth, connection, transaction);

            if (autoNumber == null)
            {
                // 新しい年月のレコードを作成
                autoNumber = new AutoNumber
                {
                    SlipType = slipType,
                    YearMonth = yearMonth,
                    LastSlipNo = 1,
                    CreatedAt = DateTime.Now,
                    CreatedBy = "system",
                    UpdatedAt = DateTime.Now,
                    UpdatedBy = "system"
                };

                var insertSql = @"
                    INSERT INTO 自動採番マスタ (
                        伝票種別コード, 年月, 最終伝票番号,
                        作成日時, 作成者名, 更新日時, 更新者名
                    ) VALUES (
                        @SlipType, @YearMonth, @LastSlipNo,
                        @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                    )";

                await connection.ExecuteAsync(insertSql, autoNumber, transaction);
            }
            else
            {
                // 番号をインクリメント
                autoNumber.LastSlipNo++;

                var updateSql = @"
                    UPDATE 自動採番マスタ SET
                        最終伝票番号 = @LastSlipNo,
                        更新日時 = CURRENT_TIMESTAMP
                    WHERE 伝票種別コード = @SlipType
                      AND 年月 = @YearMonth";

                await connection.ExecuteAsync(updateSql, autoNumber, transaction);
            }

            await transaction.CommitAsync();

            // 伝票番号を生成（例: OR20250100001）
            var slipNo = $"{slipType}{yearMonth}{autoNumber.LastSlipNo:D5}";
            return slipNo;
        }
        catch
        {
            await transaction.RollbackAsync();
            throw;
        }
    }

    /// <summary>
    /// 自動採番を削除
    /// </summary>
    public async Task DeleteAsync(string slipType, string yearMonth)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        var sql = @"
            DELETE FROM 自動採番マスタ
            WHERE 伝票種別コード = @SlipType
              AND 年月 = @YearMonth";

        await connection.ExecuteAsync(
            sql, new { SlipType = slipType, YearMonth = yearMonth });
    }
}
