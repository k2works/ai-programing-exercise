using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 発注データ明細のRepositoryクラス
    /// </summary>
    public class PurchaseOrderDetailRepository
    {
        private readonly string _connectionString;

        public PurchaseOrderDetailRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        /// <summary>
        /// 発注明細を登録
        /// </summary>
        public async Task InsertAsync(PurchaseOrderDetail detail)
        {
            const string sql = @"
                INSERT INTO 発注データ明細 (
                    発注番号, 発注行番号, 商品コード, 商品名, 発注単価, 発注数量,
                    消費税率, 入荷済数量, 値引金額, 指定納期,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @PoNo, @PoRowNo, @ProductCode, @ProductName, @UnitPrice, @Quantity,
                    @ConsumptionTaxRate, @ReceivedQuantity, @Discount, @DueDate,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, detail);
        }

        /// <summary>
        /// 発注明細を更新
        /// </summary>
        public async Task UpdateAsync(PurchaseOrderDetail detail)
        {
            const string sql = @"
                UPDATE 発注データ明細
                SET 商品コード = @ProductCode,
                    商品名 = @ProductName,
                    発注単価 = @UnitPrice,
                    発注数量 = @Quantity,
                    消費税率 = @ConsumptionTaxRate,
                    入荷済数量 = @ReceivedQuantity,
                    値引金額 = @Discount,
                    指定納期 = @DueDate,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 発注番号 = @PoNo
                  AND 発注行番号 = @PoRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, detail);
        }

        /// <summary>
        /// 発注明細を削除
        /// </summary>
        public async Task DeleteAsync(string poNo, int poRowNo)
        {
            const string sql = @"
                DELETE FROM 発注データ明細
                WHERE 発注番号 = @PoNo
                  AND 発注行番号 = @PoRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { PoNo = poNo, PoRowNo = poRowNo });
        }

        /// <summary>
        /// 発注番号と行番号で検索
        /// </summary>
        public async Task<PurchaseOrderDetail?> FindByIdAsync(string poNo, int poRowNo)
        {
            const string sql = @"
                SELECT
                    発注番号 AS PoNo,
                    発注行番号 AS PoRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    発注単価 AS UnitPrice,
                    発注数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    入荷済数量 AS ReceivedQuantity,
                    値引金額 AS Discount,
                    指定納期 AS DueDate,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 発注データ明細
                WHERE 発注番号 = @PoNo
                  AND 発注行番号 = @PoRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryFirstOrDefaultAsync<PurchaseOrderDetail>(
                sql, new { PoNo = poNo, PoRowNo = poRowNo });
        }

        /// <summary>
        /// 発注番号で検索
        /// </summary>
        public async Task<IEnumerable<PurchaseOrderDetail>> FindByPoNoAsync(string poNo)
        {
            const string sql = @"
                SELECT
                    発注番号 AS PoNo,
                    発注行番号 AS PoRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    発注単価 AS UnitPrice,
                    発注数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    入荷済数量 AS ReceivedQuantity,
                    値引金額 AS Discount,
                    指定納期 AS DueDate,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 発注データ明細
                WHERE 発注番号 = @PoNo
                ORDER BY 発注行番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<PurchaseOrderDetail>(sql, new { PoNo = poNo });
        }

        /// <summary>
        /// 商品コードで検索
        /// </summary>
        public async Task<IEnumerable<PurchaseOrderDetail>> FindByProductCodeAsync(string productCode)
        {
            const string sql = @"
                SELECT
                    発注番号 AS PoNo,
                    発注行番号 AS PoRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    発注単価 AS UnitPrice,
                    発注数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    入荷済数量 AS ReceivedQuantity,
                    値引金額 AS Discount,
                    指定納期 AS DueDate,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 発注データ明細
                WHERE 商品コード = @ProductCode
                ORDER BY 発注番号, 発注行番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<PurchaseOrderDetail>(sql, new { ProductCode = productCode });
        }
    }
}
