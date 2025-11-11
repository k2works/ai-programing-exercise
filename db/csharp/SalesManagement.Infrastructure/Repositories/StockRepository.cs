using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 在庫データのRepositoryクラス（5フィールド複合主キー）
    /// </summary>
    public class StockRepository
    {
        private readonly string _connectionString;

        public StockRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        /// <summary>
        /// 在庫を登録
        /// </summary>
        public async Task InsertAsync(Stock stock)
        {
            const string sql = @"
                INSERT INTO 在庫データ (
                    倉庫コード, 商品コード, ロット番号, 在庫区分, 良品区分,
                    実在庫数, 有効在庫数, 最終出荷日,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @WarehouseCode, @ProductCode, @LotNo, @StockType, @QualityType,
                    @ActualQuantity, @ValidQuantity, @LastDeliveryDate,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, stock);
        }

        /// <summary>
        /// 在庫を更新
        /// </summary>
        public async Task UpdateAsync(Stock stock)
        {
            const string sql = @"
                UPDATE 在庫データ
                SET 実在庫数 = @ActualQuantity,
                    有効在庫数 = @ValidQuantity,
                    最終出荷日 = @LastDeliveryDate,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 倉庫コード = @WarehouseCode
                  AND 商品コード = @ProductCode
                  AND ロット番号 = @LotNo
                  AND 在庫区分 = @StockType
                  AND 良品区分 = @QualityType";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, stock);
        }

        /// <summary>
        /// 在庫を削除
        /// </summary>
        public async Task DeleteAsync(string warehouseCode, string productCode, string lotNo,
            string stockType, string qualityType)
        {
            const string sql = @"
                DELETE FROM 在庫データ
                WHERE 倉庫コード = @WarehouseCode
                  AND 商品コード = @ProductCode
                  AND ロット番号 = @LotNo
                  AND 在庫区分 = @StockType
                  AND 良品区分 = @QualityType";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new
            {
                WarehouseCode = warehouseCode,
                ProductCode = productCode,
                LotNo = lotNo,
                StockType = stockType,
                QualityType = qualityType
            });
        }

        /// <summary>
        /// 5フィールド複合主キーで検索
        /// </summary>
        public async Task<Stock?> FindByIdAsync(string warehouseCode, string productCode, string lotNo,
            string stockType, string qualityType)
        {
            const string sql = @"
                SELECT
                    倉庫コード AS WarehouseCode,
                    商品コード AS ProductCode,
                    ロット番号 AS LotNo,
                    在庫区分 AS StockType,
                    良品区分 AS QualityType,
                    実在庫数 AS ActualQuantity,
                    有効在庫数 AS ValidQuantity,
                    最終出荷日 AS LastDeliveryDate,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 在庫データ
                WHERE 倉庫コード = @WarehouseCode
                  AND 商品コード = @ProductCode
                  AND ロット番号 = @LotNo
                  AND 在庫区分 = @StockType
                  AND 良品区分 = @QualityType";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryFirstOrDefaultAsync<Stock>(sql, new
            {
                WarehouseCode = warehouseCode,
                ProductCode = productCode,
                LotNo = lotNo,
                StockType = stockType,
                QualityType = qualityType
            });
        }

        /// <summary>
        /// すべての在庫を取得
        /// </summary>
        public async Task<IEnumerable<Stock>> FindAllAsync()
        {
            const string sql = @"
                SELECT
                    倉庫コード AS WarehouseCode,
                    商品コード AS ProductCode,
                    ロット番号 AS LotNo,
                    在庫区分 AS StockType,
                    良品区分 AS QualityType,
                    実在庫数 AS ActualQuantity,
                    有効在庫数 AS ValidQuantity,
                    最終出荷日 AS LastDeliveryDate,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 在庫データ
                ORDER BY 倉庫コード, 商品コード, ロット番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Stock>(sql);
        }

        /// <summary>
        /// 倉庫コードで検索
        /// </summary>
        public async Task<IEnumerable<Stock>> FindByWarehouseCodeAsync(string warehouseCode)
        {
            const string sql = @"
                SELECT
                    倉庫コード AS WarehouseCode,
                    商品コード AS ProductCode,
                    ロット番号 AS LotNo,
                    在庫区分 AS StockType,
                    良品区分 AS QualityType,
                    実在庫数 AS ActualQuantity,
                    有効在庫数 AS ValidQuantity,
                    最終出荷日 AS LastDeliveryDate,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 在庫データ
                WHERE 倉庫コード = @WarehouseCode
                ORDER BY 商品コード, ロット番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Stock>(sql, new { WarehouseCode = warehouseCode });
        }

        /// <summary>
        /// 商品コードで検索
        /// </summary>
        public async Task<IEnumerable<Stock>> FindByProductCodeAsync(string productCode)
        {
            const string sql = @"
                SELECT
                    倉庫コード AS WarehouseCode,
                    商品コード AS ProductCode,
                    ロット番号 AS LotNo,
                    在庫区分 AS StockType,
                    良品区分 AS QualityType,
                    実在庫数 AS ActualQuantity,
                    有効在庫数 AS ValidQuantity,
                    最終出荷日 AS LastDeliveryDate,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 在庫データ
                WHERE 商品コード = @ProductCode
                ORDER BY 倉庫コード, ロット番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Stock>(sql, new { ProductCode = productCode });
        }

        /// <summary>
        /// ロット番号で検索
        /// </summary>
        public async Task<IEnumerable<Stock>> FindByLotNoAsync(string lotNo)
        {
            const string sql = @"
                SELECT
                    倉庫コード AS WarehouseCode,
                    商品コード AS ProductCode,
                    ロット番号 AS LotNo,
                    在庫区分 AS StockType,
                    良品区分 AS QualityType,
                    実在庫数 AS ActualQuantity,
                    有効在庫数 AS ValidQuantity,
                    最終出荷日 AS LastDeliveryDate,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 在庫データ
                WHERE ロット番号 = @LotNo
                ORDER BY 倉庫コード, 商品コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Stock>(sql, new { LotNo = lotNo });
        }

        /// <summary>
        /// 倉庫コードと商品コードで在庫合計を取得
        /// </summary>
        public async Task<(int ActualQuantity, int ValidQuantity)> GetStockSummaryAsync(
            string warehouseCode, string productCode)
        {
            const string sql = @"
                SELECT
                    SUM(実在庫数) AS ActualQuantity,
                    SUM(有効在庫数) AS ValidQuantity
                FROM 在庫データ
                WHERE 倉庫コード = @WarehouseCode
                  AND 商品コード = @ProductCode
                  AND 良品区分 = 'G'";

            await using var connection = new NpgsqlConnection(_connectionString);
            var result = await connection.QueryFirstOrDefaultAsync<(int ActualQuantity, int ValidQuantity)>(
                sql, new { WarehouseCode = warehouseCode, ProductCode = productCode });
            return result;
        }

        /// <summary>
        /// 商品コード別の在庫合計を取得
        /// </summary>
        public async Task<IEnumerable<(string ProductCode, int ActualQuantity, int ValidQuantity)>>
            GetStockSummaryByProductAsync()
        {
            const string sql = @"
                SELECT
                    商品コード AS ProductCode,
                    SUM(実在庫数) AS ActualQuantity,
                    SUM(有効在庫数) AS ValidQuantity
                FROM 在庫データ
                WHERE 良品区分 = 'G'
                GROUP BY 商品コード
                ORDER BY 商品コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<(string ProductCode, int ActualQuantity, int ValidQuantity)>(sql);
        }
    }
}
