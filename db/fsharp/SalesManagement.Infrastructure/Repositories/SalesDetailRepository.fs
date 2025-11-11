module SalesManagement.Infrastructure.Repositories.SalesDetailRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findBySalesSlipNoAsync (connectionString: string) (salesSlipNo: string) : Task<SalesDetail seq> =
    task {
        let sql = """
            SELECT 売上伝票番号 AS SalesSlipNo,
                   売上行番号 AS SalesLineNo,
                   商品コード AS ProductCode,
                   商品名 AS ProductName,
                   売上数量 AS Quantity,
                   売単価 AS UnitPrice,
                   金額 AS Amount,
                   消費税 AS ConsumptionTax,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 売上データ明細
            WHERE 売上伝票番号 = @SalesSlipNo
            ORDER BY 売上行番号"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<SalesDetail>(sql, {| SalesSlipNo = salesSlipNo |})
        return results
    }

let insertAsync (connectionString: string) (detail: SalesDetail) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 売上データ明細 (
                売上伝票番号, 売上行番号, 商品コード, 商品名, 売上数量,
                売単価, 金額, 消費税,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @SalesSlipNo, @SalesLineNo, @ProductCode, @ProductName, @Quantity,
                @UnitPrice, @Amount, @ConsumptionTax,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, detail)
        return ()
    }

let deleteBySalesSlipNoAsync (connectionString: string) (salesSlipNo: string) : Task<unit> =
    task {
        let sql = "DELETE FROM 売上データ明細 WHERE 売上伝票番号 = @SalesSlipNo"
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| SalesSlipNo = salesSlipNo |})
        return ()
    }
