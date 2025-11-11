using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    public class OrderDetailRepository
    {
        private readonly string _connectionString;

        public OrderDetailRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        public async Task InsertAsync(OrderDetail orderDetail)
        {
            const string sql = @"
                INSERT INTO 受注データ明細 (
                    受注番号, 受注行番号, 商品コード, 商品名, 販売単価, 受注数量,
                    消費税率, 引当数量, 出荷指示数量, 出荷済数量, 完了フラグ, 値引金額, 納期,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @OrderNo, @OrderRowNo, @ProductCode, @ProductName, @UnitPrice, @Quantity,
                    @ConsumptionTaxRate, @ReserveQuantity, @DeliveryOrderQuantity, @DeliveredQuantity,
                    @CompleteFlag, @Discount, @DeliveryDate,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, orderDetail);
        }

        public async Task UpdateAsync(OrderDetail orderDetail)
        {
            const string sql = @"
                UPDATE 受注データ明細
                SET 商品コード = @ProductCode,
                    商品名 = @ProductName,
                    販売単価 = @UnitPrice,
                    受注数量 = @Quantity,
                    消費税率 = @ConsumptionTaxRate,
                    引当数量 = @ReserveQuantity,
                    出荷指示数量 = @DeliveryOrderQuantity,
                    出荷済数量 = @DeliveredQuantity,
                    完了フラグ = @CompleteFlag,
                    値引金額 = @Discount,
                    納期 = @DeliveryDate,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 受注番号 = @OrderNo
                  AND 受注行番号 = @OrderRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, orderDetail);
        }

        public async Task DeleteAsync(string orderNo, int orderRowNo)
        {
            const string sql = @"
                DELETE FROM 受注データ明細
                WHERE 受注番号 = @OrderNo
                  AND 受注行番号 = @OrderRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { OrderNo = orderNo, OrderRowNo = orderRowNo });
        }

        public async Task<OrderDetail?> FindByIdAsync(string orderNo, int orderRowNo)
        {
            const string sql = @"
                SELECT
                    受注番号 AS OrderNo,
                    受注行番号 AS OrderRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    販売単価 AS UnitPrice,
                    受注数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    引当数量 AS ReserveQuantity,
                    出荷指示数量 AS DeliveryOrderQuantity,
                    出荷済数量 AS DeliveredQuantity,
                    完了フラグ AS CompleteFlag,
                    値引金額 AS Discount,
                    納期 AS DeliveryDate,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 受注データ明細
                WHERE 受注番号 = @OrderNo
                  AND 受注行番号 = @OrderRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryFirstOrDefaultAsync<OrderDetail>(
                sql, new { OrderNo = orderNo, OrderRowNo = orderRowNo });
        }

        public async Task<IEnumerable<OrderDetail>> FindByOrderNoAsync(string orderNo)
        {
            const string sql = @"
                SELECT
                    受注番号 AS OrderNo,
                    受注行番号 AS OrderRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    販売単価 AS UnitPrice,
                    受注数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    引当数量 AS ReserveQuantity,
                    出荷指示数量 AS DeliveryOrderQuantity,
                    出荷済数量 AS DeliveredQuantity,
                    完了フラグ AS CompleteFlag,
                    値引金額 AS Discount,
                    納期 AS DeliveryDate,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 受注データ明細
                WHERE 受注番号 = @OrderNo
                ORDER BY 受注行番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<OrderDetail>(sql, new { OrderNo = orderNo });
        }

        public async Task<IEnumerable<OrderDetail>> FindByProductCodeAsync(string productCode)
        {
            const string sql = @"
                SELECT
                    受注番号 AS OrderNo,
                    受注行番号 AS OrderRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    販売単価 AS UnitPrice,
                    受注数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    引当数量 AS ReserveQuantity,
                    出荷指示数量 AS DeliveryOrderQuantity,
                    出荷済数量 AS DeliveredQuantity,
                    完了フラグ AS CompleteFlag,
                    値引金額 AS Discount,
                    納期 AS DeliveryDate,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 受注データ明細
                WHERE 商品コード = @ProductCode
                ORDER BY 受注番号, 受注行番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<OrderDetail>(sql, new { ProductCode = productCode });
        }
    }
}
