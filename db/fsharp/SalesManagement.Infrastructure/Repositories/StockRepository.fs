module SalesManagement.Infrastructure.Repositories.StockRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (warehouseCode: string) (productCode: string) (lotNo: string) (stockType: string) (qualityType: string) : Task<Stock option> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
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
              AND 良品区分 = @QualityType
        """
        let! result = connection.QuerySingleOrDefaultAsync<Stock>(sql, {| WarehouseCode = warehouseCode; ProductCode = productCode; LotNo = lotNo; StockType = stockType; QualityType = qualityType |})
        return if isNull (box result) then None else Some result
    }

let findByWarehouseCodeAsync (connectionString: string) (warehouseCode: string) : Task<Stock seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
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
            ORDER BY 商品コード, ロット番号
        """
        let! results = connection.QueryAsync<Stock>(sql, {| WarehouseCode = warehouseCode |})
        return results
    }

let findByProductCodeAsync (connectionString: string) (productCode: string) : Task<Stock seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
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
            ORDER BY 倉庫コード, ロット番号
        """
        let! results = connection.QueryAsync<Stock>(sql, {| ProductCode = productCode |})
        return results
    }

let insertAsync (connectionString: string) (stock: Stock) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            INSERT INTO 在庫データ (
                倉庫コード, 商品コード, ロット番号, 在庫区分, 良品区分,
                実在庫数, 有効在庫数, 最終出荷日,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @WarehouseCode, @ProductCode, @LotNo, @StockType, @QualityType,
                @ActualQuantity, @ValidQuantity, @LastDeliveryDate,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, stock)
        return ()
    }

let updateValidQuantityAsync (connectionString: string) (warehouseCode: string) (productCode: string) (lotNo: string) (stockType: string) (qualityType: string) (validQuantity: int) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 在庫データ SET
                有効在庫数 = @ValidQuantity,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 倉庫コード = @WarehouseCode
              AND 商品コード = @ProductCode
              AND ロット番号 = @LotNo
              AND 在庫区分 = @StockType
              AND 良品区分 = @QualityType
        """
        let! _ = connection.ExecuteAsync(sql, {| WarehouseCode = warehouseCode; ProductCode = productCode; LotNo = lotNo; StockType = stockType; QualityType = qualityType; ValidQuantity = validQuantity |})
        return ()
    }

let deleteAsync (connectionString: string) (warehouseCode: string) (productCode: string) (lotNo: string) (stockType: string) (qualityType: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            DELETE FROM 在庫データ
            WHERE 倉庫コード = @WarehouseCode
              AND 商品コード = @ProductCode
              AND ロット番号 = @LotNo
              AND 在庫区分 = @StockType
              AND 良品区分 = @QualityType
        """
        let! _ = connection.ExecuteAsync(sql, {| WarehouseCode = warehouseCode; ProductCode = productCode; LotNo = lotNo; StockType = stockType; QualityType = qualityType |})
        return ()
    }
