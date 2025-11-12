module SalesManagement.Infrastructure.Repositories.PurchaseDetailRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByPurchaseNoAsync (connectionString: string) (purchaseNo: string) : Task<PurchaseDetail seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                仕入番号 AS PurchaseNo,
                仕入行番号 AS PurchaseLineNo,
                商品コード AS ProductCode,
                商品名 AS ProductName,
                ロット番号 AS LotNo,
                倉庫コード AS WarehouseCode,
                仕入数量 AS PurchaseQuantity,
                仕入単価 AS UnitPrice,
                金額 AS Amount,
                消費税 AS ConsumptionTax,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 仕入データ明細
            WHERE 仕入番号 = @PurchaseNo
            ORDER BY 仕入行番号
        """
        let! results = connection.QueryAsync<PurchaseDetail>(sql, {| PurchaseNo = purchaseNo |})
        return results
    }

let insertAsync (connectionString: string) (detail: PurchaseDetail) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            INSERT INTO 仕入データ明細 (
                仕入番号, 仕入行番号, 商品コード, 商品名, ロット番号, 倉庫コード,
                仕入数量, 仕入単価, 金額, 消費税,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @PurchaseNo, @PurchaseLineNo, @ProductCode, @ProductName, @LotNo, @WarehouseCode,
                @PurchaseQuantity, @UnitPrice, @Amount, @ConsumptionTax,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, detail)
        return ()
    }

let deleteByPurchaseNoAsync (connectionString: string) (purchaseNo: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = "DELETE FROM 仕入データ明細 WHERE 仕入番号 = @PurchaseNo"
        let! _ = connection.ExecuteAsync(sql, {| PurchaseNo = purchaseNo |})
        return ()
    }
