module SalesManagement.Infrastructure.Repositories.WarehouseRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (warehouseCode: string) : Task<Warehouse option> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
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
            WHERE 倉庫コード = @WarehouseCode
        """
        let! result = connection.QuerySingleOrDefaultAsync<Warehouse>(sql, {| WarehouseCode = warehouseCode |})
        return if isNull (box result) then None else Some result
    }

let findAllAsync (connectionString: string) : Task<Warehouse seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
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
            ORDER BY 倉庫コード
        """
        let! results = connection.QueryAsync<Warehouse>(sql)
        return results
    }

let insertAsync (connectionString: string) (warehouse: Warehouse) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            INSERT INTO 倉庫マスタ (
                倉庫コード, 倉庫名, 倉庫区分, 住所, 電話番号, 責任者コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @WarehouseCode, @WarehouseName, @WarehouseType, @Address,
                @PhoneNumber, @ManagerCode,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, warehouse)
        return ()
    }

let updateAsync (connectionString: string) (warehouse: Warehouse) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 倉庫マスタ SET
                倉庫名 = @WarehouseName,
                倉庫区分 = @WarehouseType,
                住所 = @Address,
                電話番号 = @PhoneNumber,
                責任者コード = @ManagerCode,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 倉庫コード = @WarehouseCode
        """
        let! _ = connection.ExecuteAsync(sql, warehouse)
        return ()
    }

let deleteAsync (connectionString: string) (warehouseCode: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = "DELETE FROM 倉庫マスタ WHERE 倉庫コード = @WarehouseCode"
        let! _ = connection.ExecuteAsync(sql, {| WarehouseCode = warehouseCode |})
        return ()
    }
