using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    public class SupplierRepository
    {
        private readonly string _connectionString;

        public SupplierRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        public async Task InsertAsync(Supplier supplier)
        {
            const string sql = @"
                INSERT INTO 仕入先マスタ (
                    仕入先コード, 仕入先枝番, 仕入先名, 仕入先名カナ,
                    仕入先担当者名, 仕入先部門名,
                    仕入先郵便番号, 仕入先都道府県, 仕入先住所１, 仕入先住所２,
                    仕入先電話番号, 仕入先ＦＡＸ番号, 仕入先メールアドレス,
                    仕入先締日, 仕入先支払月, 仕入先支払日, 仕入先支払方法,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @SupplierCode, @SupplierBranch, @SupplierName, @SupplierNameKana,
                    @SupplierUserName, @SupplierDepartmentName,
                    @SupplierZipCode, @SupplierState, @SupplierAddress1, @SupplierAddress2,
                    @SupplierTel, @SupplierFax, @SupplierEmail,
                    @SupplierCloseDate, @SupplierPayMonths, @SupplierPayDates, @SupplierPayMethod,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, supplier);
        }

        public async Task UpdateAsync(Supplier supplier)
        {
            const string sql = @"
                UPDATE 仕入先マスタ SET
                    仕入先名 = @SupplierName,
                    仕入先名カナ = @SupplierNameKana,
                    仕入先担当者名 = @SupplierUserName,
                    仕入先部門名 = @SupplierDepartmentName,
                    仕入先郵便番号 = @SupplierZipCode,
                    仕入先都道府県 = @SupplierState,
                    仕入先住所１ = @SupplierAddress1,
                    仕入先住所２ = @SupplierAddress2,
                    仕入先電話番号 = @SupplierTel,
                    仕入先ＦＡＸ番号 = @SupplierFax,
                    仕入先メールアドレス = @SupplierEmail,
                    仕入先締日 = @SupplierCloseDate,
                    仕入先支払月 = @SupplierPayMonths,
                    仕入先支払日 = @SupplierPayDates,
                    仕入先支払方法 = @SupplierPayMethod,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 仕入先コード = @SupplierCode AND 仕入先枝番 = @SupplierBranch";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, supplier);
        }

        public async Task DeleteAsync(string supplierCode, int supplierBranch)
        {
            const string sql = "DELETE FROM 仕入先マスタ WHERE 仕入先コード = @SupplierCode AND 仕入先枝番 = @SupplierBranch";
            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { SupplierCode = supplierCode, SupplierBranch = supplierBranch });
        }

        public async Task<Supplier?> FindByIdAsync(string supplierCode, int supplierBranch)
        {
            const string sql = @"
                SELECT
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    仕入先名 AS SupplierName,
                    仕入先名カナ AS SupplierNameKana,
                    仕入先担当者名 AS SupplierUserName,
                    仕入先部門名 AS SupplierDepartmentName,
                    仕入先郵便番号 AS SupplierZipCode,
                    仕入先都道府県 AS SupplierState,
                    仕入先住所１ AS SupplierAddress1,
                    仕入先住所２ AS SupplierAddress2,
                    仕入先電話番号 AS SupplierTel,
                    仕入先ＦＡＸ番号 AS SupplierFax,
                    仕入先メールアドレス AS SupplierEmail,
                    仕入先締日 AS SupplierCloseDate,
                    仕入先支払月 AS SupplierPayMonths,
                    仕入先支払日 AS SupplierPayDates,
                    仕入先支払方法 AS SupplierPayMethod,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 仕入先マスタ
                WHERE 仕入先コード = @SupplierCode AND 仕入先枝番 = @SupplierBranch";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<Supplier>(sql, new { SupplierCode = supplierCode, SupplierBranch = supplierBranch });
        }

        public async Task<IEnumerable<Supplier>> FindBySupplierCodeAsync(string supplierCode)
        {
            const string sql = @"
                SELECT
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    仕入先名 AS SupplierName,
                    仕入先名カナ AS SupplierNameKana,
                    仕入先担当者名 AS SupplierUserName,
                    仕入先部門名 AS SupplierDepartmentName,
                    仕入先郵便番号 AS SupplierZipCode,
                    仕入先都道府県 AS SupplierState,
                    仕入先住所１ AS SupplierAddress1,
                    仕入先住所２ AS SupplierAddress2,
                    仕入先電話番号 AS SupplierTel,
                    仕入先ＦＡＸ番号 AS SupplierFax,
                    仕入先メールアドレス AS SupplierEmail,
                    仕入先締日 AS SupplierCloseDate,
                    仕入先支払月 AS SupplierPayMonths,
                    仕入先支払日 AS SupplierPayDates,
                    仕入先支払方法 AS SupplierPayMethod,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 仕入先マスタ
                WHERE 仕入先コード = @SupplierCode
                ORDER BY 仕入先枝番";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Supplier>(sql, new { SupplierCode = supplierCode });
        }

        public async Task<IEnumerable<Supplier>> FindByCompanyCodeAsync(string companyCode)
        {
            const string sql = @"
                SELECT
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    仕入先名 AS SupplierName,
                    仕入先名カナ AS SupplierNameKana,
                    仕入先担当者名 AS SupplierUserName,
                    仕入先部門名 AS SupplierDepartmentName,
                    仕入先郵便番号 AS SupplierZipCode,
                    仕入先都道府県 AS SupplierState,
                    仕入先住所１ AS SupplierAddress1,
                    仕入先住所２ AS SupplierAddress2,
                    仕入先電話番号 AS SupplierTel,
                    仕入先ＦＡＸ番号 AS SupplierFax,
                    仕入先メールアドレス AS SupplierEmail,
                    仕入先締日 AS SupplierCloseDate,
                    仕入先支払月 AS SupplierPayMonths,
                    仕入先支払日 AS SupplierPayDates,
                    仕入先支払方法 AS SupplierPayMethod,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 仕入先マスタ
                WHERE 仕入先コード = @CompanyCode
                ORDER BY 仕入先枝番";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Supplier>(sql, new { CompanyCode = companyCode });
        }
    }
}
