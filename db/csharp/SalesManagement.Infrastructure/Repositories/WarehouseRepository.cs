using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 倉庫マスタのRepositoryクラス
    /// </summary>
    public class WarehouseRepository
    {
        private readonly string _connectionString;

        public WarehouseRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        /// <summary>
        /// 倉庫を登録
        /// </summary>
        public async Task InsertAsync(Warehouse warehouse)
        {
            const string sql = @"
                INSERT INTO 倉庫マスタ (
                    倉庫コード, 倉庫名, 倉庫区分, 住所, 電話番号, 責任者コード,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @WarehouseCode, @WarehouseName, @WarehouseType, @Address, @PhoneNumber, @ManagerCode,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, warehouse);
        }

        /// <summary>
        /// 倉庫を更新
        /// </summary>
        public async Task UpdateAsync(Warehouse warehouse)
        {
            const string sql = @"
                UPDATE 倉庫マスタ
                SET 倉庫名 = @WarehouseName,
                    倉庫区分 = @WarehouseType,
                    住所 = @Address,
                    電話番号 = @PhoneNumber,
                    責任者コード = @ManagerCode,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 倉庫コード = @WarehouseCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, warehouse);
        }

        /// <summary>
        /// 倉庫を削除
        /// </summary>
        public async Task DeleteAsync(string warehouseCode)
        {
            const string sql = @"
                DELETE FROM 倉庫マスタ
                WHERE 倉庫コード = @WarehouseCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { WarehouseCode = warehouseCode });
        }

        /// <summary>
        /// 倉庫コードで検索
        /// </summary>
        public async Task<Warehouse?> FindByIdAsync(string warehouseCode)
        {
            const string sql = @"
                SELECT
                    倉庫コード AS WarehouseCode,
                    倉庫名 AS WarehouseName,
                    倉庫区分 AS WarehouseType,
                    住所 AS Address,
                    電話番号 AS PhoneNumber,
                    責任者コード AS ManagerCode,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 倉庫マスタ
                WHERE 倉庫コード = @WarehouseCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<Warehouse>(sql,
                new { WarehouseCode = warehouseCode });
        }

        /// <summary>
        /// すべての倉庫を取得
        /// </summary>
        public async Task<IEnumerable<Warehouse>> FindAllAsync()
        {
            const string sql = @"
                SELECT
                    倉庫コード AS WarehouseCode,
                    倉庫名 AS WarehouseName,
                    倉庫区分 AS WarehouseType,
                    住所 AS Address,
                    電話番号 AS PhoneNumber,
                    責任者コード AS ManagerCode,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 倉庫マスタ
                ORDER BY 倉庫コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Warehouse>(sql);
        }

        /// <summary>
        /// 倉庫区分で検索
        /// </summary>
        public async Task<IEnumerable<Warehouse>> FindByTypeAsync(int warehouseType)
        {
            const string sql = @"
                SELECT
                    倉庫コード AS WarehouseCode,
                    倉庫名 AS WarehouseName,
                    倉庫区分 AS WarehouseType,
                    住所 AS Address,
                    電話番号 AS PhoneNumber,
                    責任者コード AS ManagerCode,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 倉庫マスタ
                WHERE 倉庫区分 = @WarehouseType
                ORDER BY 倉庫コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Warehouse>(sql, new { WarehouseType = warehouseType });
        }
    }
}
