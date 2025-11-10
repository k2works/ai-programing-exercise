using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    public class CompanyCategoryRepository
    {
        private readonly string _connectionString;

        public CompanyCategoryRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        public async Task InsertAsync(CompanyCategory companyCategory)
        {
            const string sql = @"
                INSERT INTO 取引先分類マスタ (
                    分類種別, 分類, 分類名,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @CategoryTypeCode, @CategoryCode, @CategoryName,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, companyCategory);
        }

        public async Task<CompanyCategory?> FindByIdAsync(string categoryTypeCode, string categoryCode)
        {
            const string sql = @"
                SELECT
                    分類種別 AS CategoryTypeCode,
                    分類 AS CategoryCode,
                    分類名 AS CategoryName,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 取引先分類マスタ
                WHERE 分類種別 = @CategoryTypeCode AND 分類 = @CategoryCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<CompanyCategory>(sql, new { CategoryTypeCode = categoryTypeCode, CategoryCode = categoryCode });
        }

        public async Task<IEnumerable<CompanyCategory>> FindByCategoryTypeAsync(string categoryTypeCode)
        {
            const string sql = @"
                SELECT
                    分類種別 AS CategoryTypeCode,
                    分類 AS CategoryCode,
                    分類名 AS CategoryName,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 取引先分類マスタ
                WHERE 分類種別 = @CategoryTypeCode
                ORDER BY 分類";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<CompanyCategory>(sql, new { CategoryTypeCode = categoryTypeCode });
        }
    }
}
