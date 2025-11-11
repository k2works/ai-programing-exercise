using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 仕入データのRepositoryクラス
    /// </summary>
    public class PurchaseRepository
    {
        private readonly string _connectionString;

        public PurchaseRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        /// <summary>
        /// 仕入を登録
        /// </summary>
        public async Task InsertAsync(Purchase purchase)
        {
            const string sql = @"
                INSERT INTO 仕入データ (
                    仕入番号, 仕入日, 発注番号, 仕入先コード, 仕入先枝番,
                    社員コード, 倉庫コード,
                    仕入金額合計, 消費税合計, 備考,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @PurchaseNo, @PurchaseDate, @PoNo, @SupplierCode, @SupplierBranch,
                    @EmployeeCode, @WarehouseCode,
                    @PurchaseAmount, @ConsumptionTax, @SlipComment,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, purchase);
        }

        /// <summary>
        /// 仕入を更新
        /// </summary>
        public async Task UpdateAsync(Purchase purchase)
        {
            const string sql = @"
                UPDATE 仕入データ
                SET 仕入日 = @PurchaseDate,
                    発注番号 = @PoNo,
                    仕入先コード = @SupplierCode,
                    仕入先枝番 = @SupplierBranch,
                    社員コード = @EmployeeCode,
                    倉庫コード = @WarehouseCode,
                    仕入金額合計 = @PurchaseAmount,
                    消費税合計 = @ConsumptionTax,
                    備考 = @SlipComment,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 仕入番号 = @PurchaseNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, purchase);
        }

        /// <summary>
        /// 仕入を削除
        /// </summary>
        public async Task DeleteAsync(string purchaseNo)
        {
            const string sql = "DELETE FROM 仕入データ WHERE 仕入番号 = @PurchaseNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { PurchaseNo = purchaseNo });
        }

        /// <summary>
        /// 仕入番号で検索
        /// </summary>
        public async Task<Purchase?> FindByIdAsync(string purchaseNo)
        {
            const string sql = @"
                SELECT
                    仕入番号 AS PurchaseNo,
                    仕入日 AS PurchaseDate,
                    発注番号 AS PoNo,
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    社員コード AS EmployeeCode,
                    倉庫コード AS WarehouseCode,
                    仕入金額合計 AS PurchaseAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 仕入データ
                WHERE 仕入番号 = @PurchaseNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<Purchase>(sql, new { PurchaseNo = purchaseNo });
        }

        /// <summary>
        /// すべての仕入を取得
        /// </summary>
        public async Task<IEnumerable<Purchase>> FindAllAsync()
        {
            const string sql = @"
                SELECT
                    仕入番号 AS PurchaseNo,
                    仕入日 AS PurchaseDate,
                    発注番号 AS PoNo,
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    社員コード AS EmployeeCode,
                    倉庫コード AS WarehouseCode,
                    仕入金額合計 AS PurchaseAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 仕入データ
                ORDER BY 仕入日 DESC, 仕入番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Purchase>(sql);
        }

        /// <summary>
        /// 発注番号で検索
        /// </summary>
        public async Task<IEnumerable<Purchase>> FindByPoNoAsync(string poNo)
        {
            const string sql = @"
                SELECT
                    仕入番号 AS PurchaseNo,
                    仕入日 AS PurchaseDate,
                    発注番号 AS PoNo,
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    社員コード AS EmployeeCode,
                    倉庫コード AS WarehouseCode,
                    仕入金額合計 AS PurchaseAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 仕入データ
                WHERE 発注番号 = @PoNo
                ORDER BY 仕入日 DESC, 仕入番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Purchase>(sql, new { PoNo = poNo });
        }

        /// <summary>
        /// 仕入先で検索
        /// </summary>
        public async Task<IEnumerable<Purchase>> FindBySupplierAsync(string supplierCode, int supplierBranch)
        {
            const string sql = @"
                SELECT
                    仕入番号 AS PurchaseNo,
                    仕入日 AS PurchaseDate,
                    発注番号 AS PoNo,
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    社員コード AS EmployeeCode,
                    倉庫コード AS WarehouseCode,
                    仕入金額合計 AS PurchaseAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 仕入データ
                WHERE 仕入先コード = @SupplierCode
                  AND 仕入先枝番 = @SupplierBranch
                ORDER BY 仕入日 DESC, 仕入番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Purchase>(sql,
                new { SupplierCode = supplierCode, SupplierBranch = supplierBranch });
        }
    }
}
