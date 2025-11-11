using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    public class SalesDetailRepository
    {
        private readonly string _connectionString;

        public SalesDetailRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        public async Task InsertAsync(SalesDetail salesDetail)
        {
            const string sql = @"
                INSERT INTO 売上データ明細 (
                    売上番号, 売上行番号, 商品コード, 商品名, 販売単価, 売上数量,
                    消費税率, 値引金額,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @SalesNo, @SalesRowNo, @ProductCode, @ProductName, @UnitPrice, @Quantity,
                    @ConsumptionTaxRate, @Discount,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, salesDetail);
        }

        public async Task UpdateAsync(SalesDetail salesDetail)
        {
            const string sql = @"
                UPDATE 売上データ明細
                SET 商品コード = @ProductCode,
                    商品名 = @ProductName,
                    販売単価 = @UnitPrice,
                    売上数量 = @Quantity,
                    消費税率 = @ConsumptionTaxRate,
                    値引金額 = @Discount,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 売上番号 = @SalesNo
                  AND 売上行番号 = @SalesRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, salesDetail);
        }

        public async Task DeleteAsync(string salesNo, int salesRowNo)
        {
            const string sql = @"
                DELETE FROM 売上データ明細
                WHERE 売上番号 = @SalesNo
                  AND 売上行番号 = @SalesRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { SalesNo = salesNo, SalesRowNo = salesRowNo });
        }

        public async Task<SalesDetail?> FindByIdAsync(string salesNo, int salesRowNo)
        {
            const string sql = @"
                SELECT
                    売上番号 AS SalesNo,
                    売上行番号 AS SalesRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    販売単価 AS UnitPrice,
                    売上数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    値引金額 AS Discount,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 売上データ明細
                WHERE 売上番号 = @SalesNo
                  AND 売上行番号 = @SalesRowNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryFirstOrDefaultAsync<SalesDetail>(
                sql, new { SalesNo = salesNo, SalesRowNo = salesRowNo });
        }

        public async Task<IEnumerable<SalesDetail>> FindBySalesNoAsync(string salesNo)
        {
            const string sql = @"
                SELECT
                    売上番号 AS SalesNo,
                    売上行番号 AS SalesRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    販売単価 AS UnitPrice,
                    売上数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    値引金額 AS Discount,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 売上データ明細
                WHERE 売上番号 = @SalesNo
                ORDER BY 売上行番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<SalesDetail>(sql, new { SalesNo = salesNo });
        }

        public async Task<IEnumerable<SalesDetail>> FindByProductCodeAsync(string productCode)
        {
            const string sql = @"
                SELECT
                    売上番号 AS SalesNo,
                    売上行番号 AS SalesRowNo,
                    商品コード AS ProductCode,
                    商品名 AS ProductName,
                    販売単価 AS UnitPrice,
                    売上数量 AS Quantity,
                    消費税率 AS ConsumptionTaxRate,
                    値引金額 AS Discount,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 売上データ明細
                WHERE 商品コード = @ProductCode
                ORDER BY 売上番号, 売上行番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<SalesDetail>(sql, new { ProductCode = productCode });
        }
    }
}
