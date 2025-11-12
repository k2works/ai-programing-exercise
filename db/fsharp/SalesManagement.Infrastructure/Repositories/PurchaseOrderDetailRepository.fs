module SalesManagement.Infrastructure.Repositories.PurchaseOrderDetailRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByPoNoAsync (connectionString: string) (poNo: string) : Task<PurchaseOrderDetail seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                発注番号 AS PoNo,
                発注行番号 AS PoLineNo,
                商品コード AS ProductCode,
                商品名 AS ProductName,
                発注数量 AS PoQuantity,
                入荷済数量 AS ReceivedQuantity,
                仕入単価 AS UnitPrice,
                金額 AS Amount,
                消費税 AS ConsumptionTax,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 発注データ明細
            WHERE 発注番号 = @PoNo
            ORDER BY 発注行番号
        """
        let! results = connection.QueryAsync<PurchaseOrderDetail>(sql, {| PoNo = poNo |})
        return results
    }

let insertAsync (connectionString: string) (detail: PurchaseOrderDetail) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            INSERT INTO 発注データ明細 (
                発注番号, 発注行番号, 商品コード, 商品名, 発注数量, 入荷済数量,
                仕入単価, 金額, 消費税,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @PoNo, @PoLineNo, @ProductCode, @ProductName, @PoQuantity, @ReceivedQuantity,
                @UnitPrice, @Amount, @ConsumptionTax,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, detail)
        return ()
    }

let updateReceivedQuantityAsync (connectionString: string) (poNo: string) (lineNo: int) (receivedQuantity: int) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 発注データ明細 SET
                入荷済数量 = @ReceivedQuantity,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 発注番号 = @PoNo
              AND 発注行番号 = @LineNo
        """
        let! _ = connection.ExecuteAsync(sql, {| PoNo = poNo; LineNo = lineNo; ReceivedQuantity = receivedQuantity |})
        return ()
    }

let deleteByPoNoAsync (connectionString: string) (poNo: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = "DELETE FROM 発注データ明細 WHERE 発注番号 = @PoNo"
        let! _ = connection.ExecuteAsync(sql, {| PoNo = poNo |})
        return ()
    }
