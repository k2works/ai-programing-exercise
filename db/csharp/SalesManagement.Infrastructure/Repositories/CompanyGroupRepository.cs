using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    public class CompanyGroupRepository
    {
        private readonly string _connectionString;

        public CompanyGroupRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        public async Task InsertAsync(CompanyGroup group)
        {
            const string sql = @"
                INSERT INTO 取引先グループマスタ (
                    取引先グループコード, 取引先グループ名,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @CompanyGroupCode, @CompanyGroupName,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, group);
        }

        public async Task UpdateAsync(CompanyGroup group)
        {
            const string sql = @"
                UPDATE 取引先グループマスタ SET
                    取引先グループ名 = @CompanyGroupName,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 取引先グループコード = @CompanyGroupCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, group);
        }

        public async Task DeleteAsync(string companyGroupCode)
        {
            const string sql = "DELETE FROM 取引先グループマスタ WHERE 取引先グループコード = @CompanyGroupCode";
            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { CompanyGroupCode = companyGroupCode });
        }

        public async Task<CompanyGroup?> FindByIdAsync(string companyGroupCode)
        {
            const string sql = @"
                SELECT
                    取引先グループコード AS CompanyGroupCode,
                    取引先グループ名 AS CompanyGroupName,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 取引先グループマスタ
                WHERE 取引先グループコード = @CompanyGroupCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<CompanyGroup>(sql, new { CompanyGroupCode = companyGroupCode });
        }

        public async Task<IEnumerable<CompanyGroup>> FindAllAsync()
        {
            const string sql = @"
                SELECT
                    取引先グループコード AS CompanyGroupCode,
                    取引先グループ名 AS CompanyGroupName,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 取引先グループマスタ
                ORDER BY 取引先グループコード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<CompanyGroup>(sql);
        }
    }
}
