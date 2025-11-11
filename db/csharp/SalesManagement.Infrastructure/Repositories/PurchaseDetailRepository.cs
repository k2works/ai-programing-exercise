using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 仕入データ明細のRepositoryクラス
    /// </summary>
    public class PurchaseDetailRepository
    {
        private readonly string _connectionString;

        public PurchaseDetailRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        /// <summary>
        /// 仕入明細を登録
        /// </summary>
        public async Task InsertAsync(PurchaseDetail detail)
        {
            const string sql = @"
                INSERT INTO 仕入データ明細 (
                    仕入番号, 仕入行番号, 商品コード, 商品名, 仕入単価, 仕入数量,
                    消費税率, ロット番号, 倉庫コード, 値引金額,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @PurchaseNo, @PurchaseRowNo, @ProductCode, @ProductName, @UnitPrice, @Quantity,
                    @ConsumptionTaxRate, @LotNo, @WarehouseCode, @Discount,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, detail);
        }

        /// <summary>
        /// 仕入明細を更新
        /// </summary>
        public async Task UpdateAsync(PurchaseDetail detail)
        {
            const string sql = @"
                UPDATE 仕入データ明細
                SET 商品コード = @ProductCode,
                    商品名 = @ProductName,
                    仕入単価 = @UnitPrice,
                    仕入数量 = @Quantity,
                    消費税率 = @ConsumptionTaxRate,
                    ロット番号 = @LotNo,
                    倉庫コード = @WarehouseCode,
                    値引金額 = @Discount,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 仕入番号 = @PurchaseNo
                  AND 仕入行番号 = @PurchaseRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, detail);
        }

        /// <summary>
        /// 仕入明細を削除
        /// </summary>
        public async Task DeleteAsync(string purchaseNo, int purchaseRowNo)
        {
            const string sql = @"
                DELETE FROM 仕入データ明細
                WHERE 仕入番号 = @PurchaseNo
                  AND 仕入行番号 = @PurchaseRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { PurchaseNo = purchaseNo, PurchaseRowNo = purchaseRowNo });
        }

        /// <summary>
        /// 仕入番号と行番号で検索
        /// </summary>
        public async Task<PurchaseDetail?> FindByIdAsync(string purchaseNo, int purchaseRowNo)
        {
            const string sql = @"
                SELECT
                    仕入番号 AS PurchaseNo,
                    仕入行番号 AS PurchaseRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    仕入単価 AS UnitPrice,
                    仕入数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    ロット番号 AS LotNo,
                    倉庫コード AS WarehouseCode,
                    値引金額 AS Discount,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 仕入データ明細
                WHERE 仕入番号 = @PurchaseNo
                  AND 仕入行番号 = @PurchaseRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryFirstOrDefaultAsync<PurchaseDetail>(
                sql, new { PurchaseNo = purchaseNo, PurchaseRowNo = purchaseRowNo });
        }

        /// <summary>
        /// 仕入番号で検索
        /// </summary>
        public async Task<IEnumerable<PurchaseDetail>> FindByPurchaseNoAsync(string purchaseNo)
        {
            const string sql = @"
                SELECT
                    仕入番号 AS PurchaseNo,
                    仕入行番号 AS PurchaseRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    仕入単価 AS UnitPrice,
                    仕入数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    ロット番号 AS LotNo,
                    倉庫コード AS WarehouseCode,
                    値引金額 AS Discount,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 仕入データ明細
                WHERE 仕入番号 = @PurchaseNo
                ORDER BY 仕入行番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<PurchaseDetail>(sql, new { PurchaseNo = purchaseNo });
        }

        /// <summary>
        /// 商品コードで検索
        /// </summary>
        public async Task<IEnumerable<PurchaseDetail>> FindByProductCodeAsync(string productCode)
        {
            const string sql = @"
                SELECT
                    仕入番号 AS PurchaseNo,
                    仕入行番号 AS PurchaseRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    仕入単価 AS UnitPrice,
                    仕入数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    ロット番号 AS LotNo,
                    倉庫コード AS WarehouseCode,
                    値引金額 AS Discount,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 仕入データ明細
                WHERE 商品コード = @ProductCode
                ORDER BY 仕入番号, 仕入行番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<PurchaseDetail>(sql, new { ProductCode = productCode });
        }

        /// <summary>
        /// ロット番号で検索
        /// </summary>
        public async Task<IEnumerable<PurchaseDetail>> FindByLotNoAsync(string lotNo)
        {
            const string sql = @"
                SELECT
                    仕入番号 AS PurchaseNo,
                    仕入行番号 AS PurchaseRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    仕入単価 AS UnitPrice,
                    仕入数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    ロット番号 AS LotNo,
                    倉庫コード AS WarehouseCode,
                    値引金額 AS Discount,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 仕入データ明細
                WHERE ロット番号 = @LotNo
                ORDER BY 仕入番号, 仕入行番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<PurchaseDetail>(sql, new { LotNo = lotNo });
        }
    }
}
