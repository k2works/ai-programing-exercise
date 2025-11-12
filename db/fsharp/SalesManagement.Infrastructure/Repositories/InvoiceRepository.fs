module SalesManagement.Infrastructure.Repositories.InvoiceRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (invoiceNo: string) : Task<Invoice option> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                請求番号 AS InvoiceNo,
                請求日 AS InvoiceDate,
                得意先コード AS CustomerCode,
                得意先枝番 AS CustomerBranch,
                売上伝票番号 AS SalesSlipNo,
                請求額 AS InvoiceAmount,
                請求消込金額 AS ClearedAmount,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 請求データ
            WHERE 請求番号 = @InvoiceNo
        """
        let! result = connection.QuerySingleOrDefaultAsync<Invoice>(sql, {| InvoiceNo = invoiceNo |})
        return if isNull (box result) then None else Some result
    }

let findByCustomerAsync (connectionString: string) (customerCode: string) (customerBranch: int) : Task<Invoice seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                請求番号 AS InvoiceNo,
                請求日 AS InvoiceDate,
                得意先コード AS CustomerCode,
                得意先枝番 AS CustomerBranch,
                売上伝票番号 AS SalesSlipNo,
                請求額 AS InvoiceAmount,
                請求消込金額 AS ClearedAmount,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 請求データ
            WHERE 得意先コード = @CustomerCode
              AND 得意先枝番 = @CustomerBranch
            ORDER BY 請求日 DESC, 請求番号 DESC
        """
        let! results = connection.QueryAsync<Invoice>(sql,
            {| CustomerCode = customerCode; CustomerBranch = customerBranch |})
        return results
    }

let findUnclearedAsync (connectionString: string) (customerCode: string) (customerBranch: int) : Task<Invoice seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                請求番号 AS InvoiceNo,
                請求日 AS InvoiceDate,
                得意先コード AS CustomerCode,
                得意先枝番 AS CustomerBranch,
                売上伝票番号 AS SalesSlipNo,
                請求額 AS InvoiceAmount,
                請求消込金額 AS ClearedAmount,
                備考 AS Remarks,
                部門コード AS DepartmentCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 請求データ
            WHERE 得意先コード = @CustomerCode
              AND 得意先枝番 = @CustomerBranch
              AND 請求額 > 請求消込金額
            ORDER BY 請求日, 請求番号
        """
        let! results = connection.QueryAsync<Invoice>(sql,
            {| CustomerCode = customerCode; CustomerBranch = customerBranch |})
        return results
    }

let insertAsync (connectionString: string) (invoice: Invoice) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            INSERT INTO 請求データ (
                請求番号, 請求日, 得意先コード, 得意先枝番, 売上伝票番号,
                請求額, 請求消込金額, 備考, 部門コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @InvoiceNo, @InvoiceDate, @CustomerCode, @CustomerBranch, @SalesSlipNo,
                @InvoiceAmount, @ClearedAmount, @Remarks, @DepartmentCode,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, invoice)
        return ()
    }

let updateAsync (connectionString: string) (invoice: Invoice) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 請求データ SET
                請求日 = @InvoiceDate,
                得意先コード = @CustomerCode,
                得意先枝番 = @CustomerBranch,
                売上伝票番号 = @SalesSlipNo,
                請求額 = @InvoiceAmount,
                請求消込金額 = @ClearedAmount,
                備考 = @Remarks,
                部門コード = @DepartmentCode,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 請求番号 = @InvoiceNo
        """
        let! _ = connection.ExecuteAsync(sql, invoice)
        return ()
    }

let updateClearedAmountAsync (connectionString: string) (invoiceNo: string) (clearedAmount: decimal) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 請求データ SET
                請求消込金額 = 請求消込金額 + @ClearedAmount,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 請求番号 = @InvoiceNo
        """
        let! _ = connection.ExecuteAsync(sql, {| InvoiceNo = invoiceNo; ClearedAmount = clearedAmount |})
        return ()
    }

let deleteAsync (connectionString: string) (invoiceNo: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = "DELETE FROM 請求データ WHERE 請求番号 = @InvoiceNo"
        let! _ = connection.ExecuteAsync(sql, {| InvoiceNo = invoiceNo |})
        return ()
    }
