using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 商品分類マスタのRepositoryクラス
    /// </summary>
    public class ProductCategoryRepository
    {
        private readonly string _connectionString;

        public ProductCategoryRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        /// <summary>
        /// 商品分類を登録
        /// </summary>
        public async Task InsertAsync(ProductCategory category)
        {
            const string sql = @"
                INSERT INTO 商品分類マスタ (
                    商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @ProductCategoryCode, @ProductCategoryName, @ProductCategoryLevel,
                    @ProductCategoryPath, @LowestLevelFlag,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, category);
        }

        /// <summary>
        /// 商品分類を更新
        /// </summary>
        public async Task UpdateAsync(ProductCategory category)
        {
            const string sql = @"
                UPDATE 商品分類マスタ
                SET 商品分類名 = @ProductCategoryName,
                    商品分類階層 = @ProductCategoryLevel,
                    商品分類パス = @ProductCategoryPath,
                    最下層区分 = @LowestLevelFlag,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 商品分類コード = @ProductCategoryCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, category);
        }

        /// <summary>
        /// 商品分類を削除
        /// </summary>
        public async Task DeleteAsync(string productCategoryCode)
        {
            const string sql = @"
                DELETE FROM 商品分類マスタ
                WHERE 商品分類コード = @ProductCategoryCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { ProductCategoryCode = productCategoryCode });
        }

        /// <summary>
        /// 商品分類コードで検索
        /// </summary>
        public async Task<ProductCategory?> FindByIdAsync(string productCategoryCode)
        {
            const string sql = @"
                SELECT
                    商品分類コード AS ProductCategoryCode,
                    商品分類名 AS ProductCategoryName,
                    商品分類階層 AS ProductCategoryLevel,
                    商品分類パス AS ProductCategoryPath,
                    最下層区分 AS LowestLevelFlag,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 商品分類マスタ
                WHERE 商品分類コード = @ProductCategoryCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<ProductCategory>(sql,
                new { ProductCategoryCode = productCategoryCode });
        }

        /// <summary>
        /// すべての商品分類を取得
        /// </summary>
        public async Task<IEnumerable<ProductCategory>> FindAllAsync()
        {
            const string sql = @"
                SELECT
                    商品分類コード AS ProductCategoryCode,
                    商品分類名 AS ProductCategoryName,
                    商品分類階層 AS ProductCategoryLevel,
                    商品分類パス AS ProductCategoryPath,
                    最下層区分 AS LowestLevelFlag,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 商品分類マスタ
                ORDER BY 商品分類階層, 商品分類コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<ProductCategory>(sql);
        }

        /// <summary>
        /// 階層パスで配下の商品分類を検索
        /// </summary>
        public async Task<IEnumerable<ProductCategory>> FindByPathPrefixAsync(string pathPrefix)
        {
            const string sql = @"
                SELECT
                    商品分類コード AS ProductCategoryCode,
                    商品分類名 AS ProductCategoryName,
                    商品分類階層 AS ProductCategoryLevel,
                    商品分類パス AS ProductCategoryPath,
                    最下層区分 AS LowestLevelFlag,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 商品分類マスタ
                WHERE 商品分類パス LIKE @PathPrefix || '%'
                ORDER BY 商品分類階層, 商品分類コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<ProductCategory>(sql, new { PathPrefix = pathPrefix });
        }
    }
}
