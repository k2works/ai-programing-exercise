using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    public class CompanyCategoryGroupRepository
    {
        private readonly string _connectionString;

        public CompanyCategoryGroupRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        public async Task InsertAsync(CompanyCategoryGroup companyCategoryGroup)
        {
            const string sql = @"
                INSERT INTO 取引先分類所属マスタ (
                    分類種別, 分類, 取引先コード,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @CategoryTypeCode, @CategoryCode, @CompanyCode,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, companyCategoryGroup);
        }

        public async Task DeleteAsync(string categoryTypeCode, string categoryCode, string companyCode)
        {
            const string sql = @"
                DELETE FROM 取引先分類所属マスタ
                WHERE 分類種別 = @CategoryTypeCode AND 分類 = @CategoryCode AND 取引先コード = @CompanyCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { CategoryTypeCode = categoryTypeCode, CategoryCode = categoryCode, CompanyCode = companyCode });
        }

        public async Task<IEnumerable<CompanyCategoryGroup>> FindByCompanyCodeAsync(string companyCode)
        {
            const string sql = @"
                SELECT
                    分類種別 AS CategoryTypeCode,
                    分類 AS CategoryCode,
                    取引先コード AS CompanyCode,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 取引先分類所属マスタ
                WHERE 取引先コード = @CompanyCode
                ORDER BY 分類種別, 分類";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<CompanyCategoryGroup>(sql, new { CompanyCode = companyCode });
        }

        public async Task<IEnumerable<CompanyCategoryGroup>> FindByCategoryAsync(string categoryTypeCode, string categoryCode)
        {
            const string sql = @"
                SELECT
                    分類種別 AS CategoryTypeCode,
                    分類 AS CategoryCode,
                    取引先コード AS CompanyCode,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 取引先分類所属マスタ
                WHERE 分類種別 = @CategoryTypeCode AND 分類 = @CategoryCode
                ORDER BY 取引先コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<CompanyCategoryGroup>(sql, new { CategoryTypeCode = categoryTypeCode, CategoryCode = categoryCode });
        }
    }
}
