module SalesManagement.Infrastructure.Repositories.OrderDetailRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByOrderNoAsync (connectionString: string) (orderNo: string) : Task<OrderDetail seq> =
    task {
        let sql = """
            SELECT 受注番号 AS OrderNo,
                   受注行番号 AS OrderLineNo,
                   商品コード AS ProductCode,
                   商品名 AS ProductName,
                   受注数量 AS Quantity,
                   引当数量 AS AllocatedQuantity,
                   出荷指示数量 AS ShippingInstructionQuantity,
                   出荷済数量 AS ShippedQuantity,
                   売単価 AS UnitPrice,
                   金額 AS Amount,
                   消費税 AS ConsumptionTax,
                   完了フラグ AS IsCompleted,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 受注データ明細
            WHERE 受注番号 = @OrderNo
            ORDER BY 受注行番号"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<OrderDetail>(sql, {| OrderNo = orderNo |})
        return results
    }

let insertAsync (connectionString: string) (detail: OrderDetail) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 受注データ明細 (
                受注番号, 受注行番号, 商品コード, 商品名, 受注数量,
                引当数量, 出荷指示数量, 出荷済数量, 売単価, 金額, 消費税, 完了フラグ,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @OrderNo, @OrderLineNo, @ProductCode, @ProductName, @Quantity,
                @AllocatedQuantity, @ShippingInstructionQuantity, @ShippedQuantity,
                @UnitPrice, @Amount, @ConsumptionTax, @IsCompleted,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, detail)
        return ()
    }

let updateShippedQuantityAsync (connectionString: string) (orderNo: string) (lineNo: int) (shippedQuantity: int) : Task<unit> =
    task {
        let sql = """
            UPDATE 受注データ明細 SET
                出荷済数量 = @ShippedQuantity,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 受注番号 = @OrderNo
              AND 受注行番号 = @LineNo"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| OrderNo = orderNo; LineNo = lineNo; ShippedQuantity = shippedQuantity |})
        return ()
    }

let updateCompletedAsync (connectionString: string) (orderNo: string) (lineNo: int) (isCompleted: int) : Task<unit> =
    task {
        let sql = """
            UPDATE 受注データ明細 SET
                完了フラグ = @IsCompleted,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 受注番号 = @OrderNo
              AND 受注行番号 = @LineNo"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| OrderNo = orderNo; LineNo = lineNo; IsCompleted = isCompleted |})
        return ()
    }

let deleteByOrderNoAsync (connectionString: string) (orderNo: string) : Task<unit> =
    task {
        let sql = "DELETE FROM 受注データ明細 WHERE 受注番号 = @OrderNo"
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| OrderNo = orderNo |})
        return ()
    }
