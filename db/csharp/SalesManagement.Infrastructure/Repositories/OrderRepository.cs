using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    public class OrderRepository
    {
        private readonly string _connectionString;

        public OrderRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        public async Task InsertAsync(Order order)
        {
            const string sql = @"
                INSERT INTO 受注データ (
                    受注番号, 受注日, 部門コード, 開始日, 顧客コード, 顧客枝番,
                    社員コード, 希望納期, 客先注文番号, 倉庫コード,
                    受注金額合計, 消費税合計, 備考,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @OrderNo, @OrderDate, @DepartmentCode, @StartDate, @CustomerCode, @CustomerBranch,
                    @EmployeeCode, @RequiredDate, @CustomerOrderNo, @WarehouseCode,
                    @OrderAmount, @ConsumptionTax, @SlipComment,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, order);
        }

        public async Task UpdateAsync(Order order)
        {
            const string sql = @"
                UPDATE 受注データ
                SET 受注日 = @OrderDate,
                    部門コード = @DepartmentCode,
                    開始日 = @StartDate,
                    顧客コード = @CustomerCode,
                    顧客枝番 = @CustomerBranch,
                    社員コード = @EmployeeCode,
                    希望納期 = @RequiredDate,
                    客先注文番号 = @CustomerOrderNo,
                    倉庫コード = @WarehouseCode,
                    受注金額合計 = @OrderAmount,
                    消費税合計 = @ConsumptionTax,
                    備考 = @SlipComment,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 受注番号 = @OrderNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, order);
        }

        public async Task DeleteAsync(string orderNo)
        {
            const string sql = "DELETE FROM 受注データ WHERE 受注番号 = @OrderNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { OrderNo = orderNo });
        }

        public async Task<Order?> FindByIdAsync(string orderNo)
        {
            const string sql = @"
                SELECT
                    受注番号 AS OrderNo,
                    受注日 AS OrderDate,
                    部門コード AS DepartmentCode,
                    開始日 AS StartDate,
                    顧客コード AS CustomerCode,
                    顧客枝番 AS CustomerBranch,
                    社員コード AS EmployeeCode,
                    希望納期 AS RequiredDate,
                    客先注文番号 AS CustomerOrderNo,
                    倉庫コード AS WarehouseCode,
                    受注金額合計 AS OrderAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 受注データ
                WHERE 受注番号 = @OrderNo";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryFirstOrDefaultAsync<Order>(sql, new { OrderNo = orderNo });
        }

        public async Task<IEnumerable<Order>> FindAllAsync()
        {
            const string sql = @"
                SELECT
                    受注番号 AS OrderNo,
                    受注日 AS OrderDate,
                    部門コード AS DepartmentCode,
                    開始日 AS StartDate,
                    顧客コード AS CustomerCode,
                    顧客枝番 AS CustomerBranch,
                    社員コード AS EmployeeCode,
                    希望納期 AS RequiredDate,
                    客先注文番号 AS CustomerOrderNo,
                    倉庫コード AS WarehouseCode,
                    受注金額合計 AS OrderAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 受注データ
                ORDER BY 受注日 DESC, 受注番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Order>(sql);
        }

        public async Task<IEnumerable<Order>> FindByCustomerCodeAsync(string customerCode)
        {
            const string sql = @"
                SELECT
                    受注番号 AS OrderNo,
                    受注日 AS OrderDate,
                    部門コード AS DepartmentCode,
                    開始日 AS StartDate,
                    顧客コード AS CustomerCode,
                    顧客枝番 AS CustomerBranch,
                    社員コード AS EmployeeCode,
                    希望納期 AS RequiredDate,
                    客先注文番号 AS CustomerOrderNo,
                    倉庫コード AS WarehouseCode,
                    受注金額合計 AS OrderAmount,
                    消費税合計 AS ConsumptionTax,
                    備考 AS SlipComment,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 受注データ
                WHERE 顧客コード = @CustomerCode
                ORDER BY 受注日 DESC, 受注番号";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Order>(sql, new { CustomerCode = customerCode });
        }
    }
}
