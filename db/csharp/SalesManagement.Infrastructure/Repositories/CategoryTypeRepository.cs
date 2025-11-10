using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    public class CategoryTypeRepository
    {
        private readonly string _connectionString;

        public CategoryTypeRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        public async Task InsertAsync(CategoryType categoryType)
        {
            const string sql = @"
                INSERT INTO 分類種別マスタ (
                    分類種別, 分類種別名,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @CategoryTypeCode, @CategoryTypeName,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, categoryType);
        }

        public async Task<CategoryType?> FindByIdAsync(string categoryTypeCode)
        {
            const string sql = @"
                SELECT
                    分類種別 AS CategoryTypeCode,
                    分類種別名 AS CategoryTypeName,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 分類種別マスタ
                WHERE 分類種別 = @CategoryTypeCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<CategoryType>(sql, new { CategoryTypeCode = categoryTypeCode });
        }

        public async Task<IEnumerable<CategoryType>> FindAllAsync()
        {
            const string sql = @"
                SELECT
                    分類種別 AS CategoryTypeCode,
                    分類種別名 AS CategoryTypeName,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 分類種別マスタ
                ORDER BY 分類種別";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<CategoryType>(sql);
        }
    }
}
