using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 代替商品のRepositoryクラス
    /// </summary>
    public class AlternateProductRepository
    {
        private readonly string _connectionString;

        public AlternateProductRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        /// <summary>
        /// 代替商品を登録
        /// </summary>
        public async Task InsertAsync(AlternateProduct alternate)
        {
            const string sql = @"
                INSERT INTO 代替商品 (
                    商品コード, 代替商品コード, 優先順位,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @ProductCode, @AlternateProductCode, @Priority,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, alternate);
        }

        /// <summary>
        /// 代替商品を更新
        /// </summary>
        public async Task UpdateAsync(AlternateProduct alternate)
        {
            const string sql = @"
                UPDATE 代替商品
                SET 優先順位 = @Priority,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 商品コード = @ProductCode
                  AND 代替商品コード = @AlternateProductCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, alternate);
        }

        /// <summary>
        /// 代替商品を削除
        /// </summary>
        public async Task DeleteAsync(string productCode, string alternateProductCode)
        {
            const string sql = @"
                DELETE FROM 代替商品
                WHERE 商品コード = @ProductCode
                  AND 代替商品コード = @AlternateProductCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { ProductCode = productCode, AlternateProductCode = alternateProductCode });
        }

        /// <summary>
        /// 商品コードで代替商品を検索
        /// </summary>
        public async Task<IEnumerable<AlternateProduct>> FindByProductCodeAsync(string productCode)
        {
            const string sql = @"
                SELECT
                    商品コード AS ProductCode,
                    代替商品コード AS AlternateProductCode,
                    優先順位 AS Priority,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 代替商品
                WHERE 商品コード = @ProductCode
                ORDER BY 優先順位, 代替商品コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<AlternateProduct>(sql, new { ProductCode = productCode });
        }

        /// <summary>
        /// 商品コードで代替商品情報を取得（商品マスタ結合）
        /// </summary>
        public async Task<IEnumerable<dynamic>> FindAlternatesWithProductInfoAsync(string productCode)
        {
            const string sql = @"
                SELECT
                    a.商品コード,
                    a.代替商品コード,
                    a.優先順位,
                    p.商品正式名,
                    p.販売単価
                FROM 代替商品 a
                INNER JOIN 商品マスタ p ON a.代替商品コード = p.商品コード
                WHERE a.商品コード = @ProductCode
                ORDER BY a.優先順位, a.代替商品コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync(sql, new { ProductCode = productCode });
        }
    }
}
