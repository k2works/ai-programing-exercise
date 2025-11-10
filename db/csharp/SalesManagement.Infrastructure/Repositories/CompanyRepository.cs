using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    public class CompanyRepository
    {
        private readonly string _connectionString;

        public CompanyRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        public async Task InsertAsync(Company company)
        {
            const string sql = @"
                INSERT INTO 取引先マスタ (
                    取引先コード, 取引先名, 取引先名カナ, 仕入先区分,
                    郵便番号, 都道府県, 住所１, 住所２,
                    取引禁止フラグ, 雑区分, 取引先グループコード,
                    与信限度額, 与信一時増加枠,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @CompanyCode, @CompanyName, @CompanyNameKana, @SupplierType,
                    @ZipCode, @State, @Address1, @Address2,
                    @NoSalesFlag, @WideUseType, @CompanyGroupCode,
                    @MaxCredit, @TempCreditUp,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, company);
        }

        public async Task UpdateAsync(Company company)
        {
            const string sql = @"
                UPDATE 取引先マスタ SET
                    取引先名 = @CompanyName,
                    取引先名カナ = @CompanyNameKana,
                    仕入先区分 = @SupplierType,
                    郵便番号 = @ZipCode,
                    都道府県 = @State,
                    住所１ = @Address1,
                    住所２ = @Address2,
                    取引禁止フラグ = @NoSalesFlag,
                    雑区分 = @WideUseType,
                    取引先グループコード = @CompanyGroupCode,
                    与信限度額 = @MaxCredit,
                    与信一時増加枠 = @TempCreditUp,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 取引先コード = @CompanyCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, company);
        }

        public async Task DeleteAsync(string companyCode)
        {
            const string sql = "DELETE FROM 取引先マスタ WHERE 取引先コード = @CompanyCode";
            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { CompanyCode = companyCode });
        }

        public async Task<Company?> FindByIdAsync(string companyCode)
        {
            const string sql = @"
                SELECT
                    取引先コード AS CompanyCode,
                    取引先名 AS CompanyName,
                    取引先名カナ AS CompanyNameKana,
                    仕入先区分 AS SupplierType,
                    郵便番号 AS ZipCode,
                    都道府県 AS State,
                    住所１ AS Address1,
                    住所２ AS Address2,
                    取引禁止フラグ AS NoSalesFlag,
                    雑区分 AS WideUseType,
                    取引先グループコード AS CompanyGroupCode,
                    与信限度額 AS MaxCredit,
                    与信一時増加枠 AS TempCreditUp,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 取引先マスタ
                WHERE 取引先コード = @CompanyCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<Company>(sql, new { CompanyCode = companyCode });
        }

        public async Task<IEnumerable<Company>> FindAllAsync()
        {
            const string sql = @"
                SELECT
                    取引先コード AS CompanyCode,
                    取引先名 AS CompanyName,
                    取引先名カナ AS CompanyNameKana,
                    仕入先区分 AS SupplierType,
                    郵便番号 AS ZipCode,
                    都道府県 AS State,
                    住所１ AS Address1,
                    住所２ AS Address2,
                    取引禁止フラグ AS NoSalesFlag,
                    雑区分 AS WideUseType,
                    取引先グループコード AS CompanyGroupCode,
                    与信限度額 AS MaxCredit,
                    与信一時増加枠 AS TempCreditUp,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 取引先マスタ
                ORDER BY 取引先コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Company>(sql);
        }

        public async Task<IEnumerable<Company>> FindByGroupCodeAsync(string companyGroupCode)
        {
            const string sql = @"
                SELECT
                    取引先コード AS CompanyCode,
                    取引先名 AS CompanyName,
                    取引先名カナ AS CompanyNameKana,
                    仕入先区分 AS SupplierType,
                    郵便番号 AS ZipCode,
                    都道府県 AS State,
                    住所１ AS Address1,
                    住所２ AS Address2,
                    取引禁止フラグ AS NoSalesFlag,
                    雑区分 AS WideUseType,
                    取引先グループコード AS CompanyGroupCode,
                    与信限度額 AS MaxCredit,
                    与信一時増加枠 AS TempCreditUp,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 取引先マスタ
                WHERE 取引先グループコード = @CompanyGroupCode
                ORDER BY 取引先コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Company>(sql, new { CompanyGroupCode = companyGroupCode });
        }
    }
}
