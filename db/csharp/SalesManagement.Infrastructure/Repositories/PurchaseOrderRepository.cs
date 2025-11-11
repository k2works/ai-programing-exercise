using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 発注データのRepositoryクラス
    /// </summary>
    public class PurchaseOrderRepository
    {
        private readonly string _connectionString;

        public PurchaseOrderRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        /// <summary>
        /// 発注を登録
        /// </summary>
        public async Task InsertAsync(PurchaseOrder purchaseOrder)
        {
            const string sql = @"
                INSERT INTO 発注データ (
                    発注番号, 発注日, 受注番号, 仕入先コード, 仕入先枝番,
                    社員コード, 指定納期, 倉庫コード,
                    発注金額合計, 消費税合計, 備考,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @PoNo, @PoDate, @OrderNo, @SupplierCode, @SupplierBranch,
                    @EmployeeCode, @DueDate, @WarehouseCode,
                    @PoAmount, @ConsumptionTax, @SlipComment,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, purchaseOrder);
        }

        /// <summary>
        /// 発注を更新
        /// </summary>
        public async Task UpdateAsync(PurchaseOrder purchaseOrder)
        {
            const string sql = @"
                UPDATE 発注データ
                SET 発注日 = @PoDate,
                    受注番号 = @OrderNo,
                    仕入先コード = @SupplierCode,
                    仕入先枝番 = @SupplierBranch,
                    社員コード = @EmployeeCode,
                    指定納期 = @DueDate,
                    倉庫コード = @WarehouseCode,
                    発注金額合計 = @PoAmount,
                    消費税合計 = @ConsumptionTax,
                    備考 = @SlipComment,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 発注番号 = @PoNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, purchaseOrder);
        }

        /// <summary>
        /// 発注を削除
        /// </summary>
        public async Task DeleteAsync(string poNo)
        {
            const string sql = "DELETE FROM 発注データ WHERE 発注番号 = @PoNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { PoNo = poNo });
        }

        /// <summary>
        /// 発注番号で検索
        /// </summary>
        public async Task<PurchaseOrder?> FindByIdAsync(string poNo)
        {
            const string sql = @"
                SELECT
                    発注番号 AS PoNo,
                    発注日 AS PoDate,
                    受注番号 AS OrderNo,
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    社員コード AS EmployeeCode,
                    指定納期 AS DueDate,
                    倉庫コード AS WarehouseCode,
                    発注金額合計 AS PoAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 発注データ
                WHERE 発注番号 = @PoNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<PurchaseOrder>(sql, new { PoNo = poNo });
        }

        /// <summary>
        /// すべての発注を取得
        /// </summary>
        public async Task<IEnumerable<PurchaseOrder>> FindAllAsync()
        {
            const string sql = @"
                SELECT
                    発注番号 AS PoNo,
                    発注日 AS PoDate,
                    受注番号 AS OrderNo,
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    社員コード AS EmployeeCode,
                    指定納期 AS DueDate,
                    倉庫コード AS WarehouseCode,
                    発注金額合計 AS PoAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 発注データ
                ORDER BY 発注日 DESC, 発注番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<PurchaseOrder>(sql);
        }

        /// <summary>
        /// 受注番号で検索
        /// </summary>
        public async Task<IEnumerable<PurchaseOrder>> FindByOrderNoAsync(string orderNo)
        {
            const string sql = @"
                SELECT
                    発注番号 AS PoNo,
                    発注日 AS PoDate,
                    受注番号 AS OrderNo,
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    社員コード AS EmployeeCode,
                    指定納期 AS DueDate,
                    倉庫コード AS WarehouseCode,
                    発注金額合計 AS PoAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 発注データ
                WHERE 受注番号 = @OrderNo
                ORDER BY 発注日 DESC, 発注番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<PurchaseOrder>(sql, new { OrderNo = orderNo });
        }

        /// <summary>
        /// 仕入先で検索
        /// </summary>
        public async Task<IEnumerable<PurchaseOrder>> FindBySupplierAsync(string supplierCode, int supplierBranch)
        {
            const string sql = @"
                SELECT
                    発注番号 AS PoNo,
                    発注日 AS PoDate,
                    受注番号 AS OrderNo,
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    社員コード AS EmployeeCode,
                    指定納期 AS DueDate,
                    倉庫コード AS WarehouseCode,
                    発注金額合計 AS PoAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 発注データ
                WHERE 仕入先コード = @SupplierCode
                  AND 仕入先枝番 = @SupplierBranch
                ORDER BY 発注日 DESC, 発注番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<PurchaseOrder>(sql,
                new { SupplierCode = supplierCode, SupplierBranch = supplierBranch });
        }
    }
}
