using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    public class SalesRepository
    {
        private readonly string _connectionString;

        public SalesRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        public async Task InsertAsync(Sales sales)
        {
            const string sql = @"
                INSERT INTO 売上データ (
                    売上番号, 売上日, 受注番号, 部門コード, 開始日,
                    取引先コード, 社員コード, 倉庫コード,
                    売上金額合計, 消費税合計, 備考,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @SalesNo, @SalesDate, @OrderNo, @DepartmentCode, @StartDate,
                    @CompanyCode, @EmployeeCode, @WarehouseCode,
                    @SalesAmount, @ConsumptionTax, @SlipComment,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, sales);
        }

        public async Task UpdateAsync(Sales sales)
        {
            const string sql = @"
                UPDATE 売上データ
                SET 売上日 = @SalesDate,
                    受注番号 = @OrderNo,
                    部門コード = @DepartmentCode,
                    開始日 = @StartDate,
                    取引先コード = @CompanyCode,
                    社員コード = @EmployeeCode,
                    倉庫コード = @WarehouseCode,
                    売上金額合計 = @SalesAmount,
                    消費税合計 = @ConsumptionTax,
                    備考 = @SlipComment,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 売上番号 = @SalesNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, sales);
        }

        public async Task DeleteAsync(string salesNo)
        {
            const string sql = "DELETE FROM 売上データ WHERE 売上番号 = @SalesNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { SalesNo = salesNo });
        }

        public async Task<Sales?> FindByIdAsync(string salesNo)
        {
            const string sql = @"
                SELECT
                    売上番号 AS SalesNo,
                    売上日 AS SalesDate,
                    受注番号 AS OrderNo,
                    部門コード AS DepartmentCode,
                    開始日 AS StartDate,
                    取引先コード AS CompanyCode,
                    社員コード AS EmployeeCode,
                    倉庫コード AS WarehouseCode,
                    売上金額合計 AS SalesAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 売上データ
                WHERE 売上番号 = @SalesNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryFirstOrDefaultAsync<Sales>(sql, new { SalesNo = salesNo });
        }

        public async Task<IEnumerable<Sales>> FindAllAsync()
        {
            const string sql = @"
                SELECT
                    売上番号 AS SalesNo,
                    売上日 AS SalesDate,
                    受注番号 AS OrderNo,
                    部門コード AS DepartmentCode,
                    開始日 AS StartDate,
                    取引先コード AS CompanyCode,
                    社員コード AS EmployeeCode,
                    倉庫コード AS WarehouseCode,
                    売上金額合計 AS SalesAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 売上データ
                ORDER BY 売上日 DESC, 売上番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Sales>(sql);
        }

        public async Task<IEnumerable<Sales>> FindByOrderNoAsync(string orderNo)
        {
            const string sql = @"
                SELECT
                    売上番号 AS SalesNo,
                    売上日 AS SalesDate,
                    受注番号 AS OrderNo,
                    部門コード AS DepartmentCode,
                    開始日 AS StartDate,
                    取引先コード AS CompanyCode,
                    社員コード AS EmployeeCode,
                    倉庫コード AS WarehouseCode,
                    売上金額合計 AS SalesAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 売上データ
                WHERE 受注番号 = @OrderNo
                ORDER BY 売上日 DESC, 売上番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Sales>(sql, new { OrderNo = orderNo });
        }

        public async Task<IEnumerable<Sales>> FindByCompanyCodeAsync(string companyCode)
        {
            const string sql = @"
                SELECT
                    売上番号 AS SalesNo,
                    売上日 AS SalesDate,
                    受注番号 AS OrderNo,
                    部門コード AS DepartmentCode,
                    開始日 AS StartDate,
                    取引先コード AS CompanyCode,
                    社員コード AS EmployeeCode,
                    倉庫コード AS WarehouseCode,
                    売上金額合計 AS SalesAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 売上データ
                WHERE 取引先コード = @CompanyCode
                ORDER BY 売上日 DESC, 売上番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Sales>(sql, new { CompanyCode = companyCode });
        }
    }
}
