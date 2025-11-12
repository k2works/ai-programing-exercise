module SalesManagement.Infrastructure.Repositories.InvoiceDetailRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByInvoiceNoAsync (connectionString: string) (invoiceNo: string) : Task<InvoiceDetail seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                請求番号 AS InvoiceNo,
                請求明細番号 AS InvoiceDetailNo,
                売上伝票番号 AS SalesSlipNo,
                売上明細番号 AS SalesDetailNo,
                請求額 AS InvoiceAmount,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 請求データ明細
            WHERE 請求番号 = @InvoiceNo
            ORDER BY 請求明細番号
        """
        let! results = connection.QueryAsync<InvoiceDetail>(sql, {| InvoiceNo = invoiceNo |})
        return results
    }

let insertAsync (connectionString: string) (detail: InvoiceDetail) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            INSERT INTO 請求データ明細 (
                請求番号, 請求明細番号, 売上伝票番号, 売上明細番号, 請求額,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @InvoiceNo, @InvoiceDetailNo, @SalesSlipNo, @SalesDetailNo, @InvoiceAmount,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, detail)
        return ()
    }

let deleteByInvoiceNoAsync (connectionString: string) (invoiceNo: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = "DELETE FROM 請求データ明細 WHERE 請求番号 = @InvoiceNo"
        let! _ = connection.ExecuteAsync(sql, {| InvoiceNo = invoiceNo |})
        return ()
    }
