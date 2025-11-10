using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 顧客別販売単価のRepositoryクラス
    /// </summary>
    public class PriceByCustomerRepository
    {
        private readonly string _connectionString;

        public PriceByCustomerRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        /// <summary>
        /// 顧客別販売単価を登録
        /// </summary>
        public async Task InsertAsync(PriceByCustomer price)
        {
            const string sql = @"
                INSERT INTO 顧客別販売単価 (
                    商品コード, 取引先コード, 販売単価,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @ProductCode, @CustomerCode, @SellingPrice,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, price);
        }

        /// <summary>
        /// 顧客別販売単価を更新
        /// </summary>
        public async Task UpdateAsync(PriceByCustomer price)
        {
            const string sql = @"
                UPDATE 顧客別販売単価
                SET 販売単価 = @SellingPrice,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 商品コード = @ProductCode
                  AND 取引先コード = @CustomerCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, price);
        }

        /// <summary>
        /// 顧客別販売単価を削除
        /// </summary>
        public async Task DeleteAsync(string productCode, string customerCode)
        {
            const string sql = @"
                DELETE FROM 顧客別販売単価
                WHERE 商品コード = @ProductCode
                  AND 取引先コード = @CustomerCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { ProductCode = productCode, CustomerCode = customerCode });
        }

        /// <summary>
        /// 複合主キーで検索
        /// </summary>
        public async Task<PriceByCustomer?> FindByIdAsync(string productCode, string customerCode)
        {
            const string sql = @"
                SELECT
                    商品コード AS ProductCode,
                    取引先コード AS CustomerCode,
                    販売単価 AS SellingPrice,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 顧客別販売単価
                WHERE 商品コード = @ProductCode
                  AND 取引先コード = @CustomerCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<PriceByCustomer>(sql,
                new { ProductCode = productCode, CustomerCode = customerCode });
        }

        /// <summary>
        /// 商品コードで顧客別販売単価を検索
        /// </summary>
        public async Task<IEnumerable<PriceByCustomer>> FindByProductCodeAsync(string productCode)
        {
            const string sql = @"
                SELECT
                    商品コード AS ProductCode,
                    取引先コード AS CustomerCode,
                    販売単価 AS SellingPrice,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 顧客別販売単価
                WHERE 商品コード = @ProductCode
                ORDER BY 取引先コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<PriceByCustomer>(sql, new { ProductCode = productCode });
        }

        /// <summary>
        /// 取引先コードで顧客別販売単価を検索
        /// </summary>
        public async Task<IEnumerable<PriceByCustomer>> FindByCustomerCodeAsync(string customerCode)
        {
            const string sql = @"
                SELECT
                    商品コード AS ProductCode,
                    取引先コード AS CustomerCode,
                    販売単価 AS SellingPrice,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 顧客別販売単価
                WHERE 取引先コード = @CustomerCode
                ORDER BY 商品コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<PriceByCustomer>(sql, new { CustomerCode = customerCode });
        }
    }
}
